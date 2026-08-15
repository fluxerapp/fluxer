// SPDX-License-Identifier: AGPL-3.0-or-later

function nativeFileName(platform = process.platform, arch = process.arch) {
	switch (platform) {
		case 'darwin':
			if (arch === 'x64' || arch === 'arm64') return `platform-info.darwin-${arch}.node`;
			break;
		case 'win32':
			if (arch === 'x64' || arch === 'arm64') return `platform-info.win32-${arch}-msvc.node`;
			break;
		case 'linux':
			if (arch === 'x64' || arch === 'arm64') return `platform-info.linux-${arch}-gnu.node`;
			break;
	}
	throw new Error(`Unsupported platform-info target: ${platform}/${arch}`);
}

module.exports = {
	nativeFileName,
};
