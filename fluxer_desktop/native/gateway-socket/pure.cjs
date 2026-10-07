// SPDX-License-Identifier: AGPL-3.0-or-later

function nativeFileName(platform = process.platform, arch = process.arch) {
	switch (platform) {
		case 'darwin':
			if (arch === 'x64' || arch === 'arm64') return `gateway-socket.darwin-${arch}.node`;
			break;
		case 'win32':
			if (arch === 'x64' || arch === 'arm64') return `gateway-socket.win32-${arch}-msvc.node`;
			break;
		case 'linux':
			if (arch === 'x64' || arch === 'arm64') return `gateway-socket.linux-${arch}-gnu.node`;
			break;
	}
	throw new Error(`Unsupported gateway-socket target: ${platform}/${arch}`);
}

module.exports = {
	nativeFileName,
};
