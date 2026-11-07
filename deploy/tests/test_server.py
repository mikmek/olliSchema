"""Release safety tests without Docker or server access; also runnable on Windows."""
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import types
import unittest
from unittest.mock import patch

try:
    import fcntl
except ImportError:
    sys.modules['fcntl'] = types.SimpleNamespace()

spec = importlib.util.spec_from_file_location('schema_deploy', Path(__file__).parents[1] / 'server.py')
tool = importlib.util.module_from_spec(spec)
spec.loader.exec_module(tool)


class ReleaseTests(unittest.TestCase):
    release = '20261107T120000Z-aaaaaaaaaaaa'

    def test_redis_is_private_and_readiness_uses_http(self):
        with patch.object(tool, 'bind_address', return_value='192.168.10.30'):
            config = tool.make_compose(self.release, 'olli-schema:' + self.release)
        self.assertNotIn('ports', config['services']['schema-redis'])
        self.assertIn('/health/ready', config['services']['schema']['healthcheck']['test'][-1])
        self.assertTrue(config['volumes']['schema-redis-data']['external'])

    def fixture(self, root, rollback_safe=False):
        for key, relative in [('BASE', 'deploy'), ('ARCHIVES', 'archives'),
                              ('INCOMING', 'incoming'), ('CONFIG', 'config')]:
            self.enterContext(patch.object(tool, key, root / relative))
            (root / relative).mkdir()
        self.enterContext(patch.object(tool, 'STATE', tool.BASE / 'state.json'))
        self.enterContext(patch.object(tool, 'LOG', None))
        (tool.CONFIG / 'olli-schema.conf').write_text('configuration')
        source = tool.INCOMING / self.release
        source.mkdir()
        (source / 'image.tar.gz').write_bytes(b'archive content for checksum test')
        manifest = {'release': self.release, 'image': 'olli-schema:' + self.release,
                    'sourceCommit': 'a' * 40, 'eventCoreCommit': 'b' * 40, 'dirty': False,
                    'archiveSha256': tool.sha256(source / 'image.tar.gz'),
                    'rollbackSafe': rollback_safe}
        (source / 'manifest.json').write_text(json.dumps(manifest))
        return source, manifest

    def test_corrupt_transfer_never_loads_image_or_changes_state(self):
        with tempfile.TemporaryDirectory() as temporary:
            source, manifest = self.fixture(Path(temporary))
            (source / 'image.tar.gz').write_bytes(b'corrupted')
            with patch.object(tool, 'run') as command:
                with self.assertRaisesRegex(RuntimeError, 'checksum mismatch'):
                    tool.apply(self.release)
                command.assert_not_called()
            self.assertFalse(tool.STATE.exists())

    def test_failed_compatible_release_restores_previous_without_changing_state(self):
        with tempfile.TemporaryDirectory() as temporary:
            self.fixture(Path(temporary), rollback_safe=True)
            tool.write_json(tool.STATE, {'current': 'previous-release'})
            info = [{'Id': 'loaded-id', 'Config': {'Labels': {
                'org.opencontainers.image.revision': 'a' * 40,
                'de.bizgrid.eventcore.revision': 'b' * 40,
                'de.bizgrid.release': self.release}}}]
            with patch.object(tool, 'run', return_value=json.dumps(info)), \
                 patch.object(tool, 'bind_address', return_value='192.168.10.30'), \
                 patch.object(tool, 'compose'), patch.object(tool, 'load_release') as load, \
                 patch.object(tool, 'activate', side_effect=[RuntimeError('not ready'), None]) as activate:
                with self.assertRaisesRegex(RuntimeError, 'not ready'):
                    tool.apply(self.release)
                load.assert_called_once_with('previous-release')
                self.assertEqual(activate.call_args_list[-1].args, ('previous-release',))
            self.assertEqual(tool.state()['current'], 'previous-release')

    def test_dirty_source_is_rejected_before_release_directory_is_created(self):
        with tempfile.TemporaryDirectory() as temporary:
            source, manifest = self.fixture(Path(temporary))
            manifest['dirty'] = True
            (source / 'manifest.json').write_text(json.dumps(manifest))
            with self.assertRaisesRegex(RuntimeError, 'clean, committed'):
                tool.apply(self.release)
            self.assertFalse((tool.ARCHIVES / self.release).exists())


if __name__ == '__main__':
    unittest.main()
