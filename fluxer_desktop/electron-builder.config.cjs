// SPDX-License-Identifier: AGPL-3.0-or-later

const isCanary = process.env.BUILD_CHANNEL === 'canary';
const {execFile} = require('node:child_process');
const fs = require('node:fs/promises');
const os = require('node:os');
const path = require('node:path');
const {promisify} = require('node:util');
const execFileAsync = promisify(execFile);
const productName = isCanary ? 'Fluxer Canary' : 'Fluxer';
const appId = isCanary ? 'app.fluxer.canary' : 'app.fluxer';
const iconDir = isCanary ? 'icons-canary' : 'icons-stable';
const packageName = isCanary ? 'fluxer_desktop_canary' : 'fluxer_desktop';
const linuxPackageName = isCanary ? 'fluxer-canary' : 'fluxer';
const desktopBuildVariant = process.env.FLUXER_DESKTOP_BUILD_VARIANT || process.env.DESKTOP_VARIANT || 'default';
const windowsGameCaptureModuleEnabled =
	desktopBuildVariant === 'windows-game-capture' || process.env.FLUXER_WINDOWS_GAME_CAPTURE_MODULE_ENABLED === 'true';
const linuxDesktopActionIds = ['open-settings', 'new-dm'];
const linuxDesktopActionList = `${linuxDesktopActionIds.join(';')};`;
const rpmBuildIdFilePrefix = '/usr/lib/.build-id';
const rpmBuildIdLinkFpmArgs = [
	'--rpm-rpmbuild-define',
	'_build_id_links none',
	'--rpm-rpmbuild-define',
	'_missing_build_ids_terminate_build 0',
];
const macOSMinimumSystemVersion = '12.0';
const isLinuxBuild = process.argv.includes('--linux');
const isMacBuild = process.argv.includes('--mac');
const isWindowsBuild = process.argv.includes('--win');
const targetPlatform = isLinuxBuild ? 'linux' : isMacBuild ? 'darwin' : isWindowsBuild ? 'win32' : process.platform;
const metadataName = isLinuxBuild ? linuxPackageName : packageName;
const provisioningProfile = isCanary
	? 'build_resources/profiles/Fluxer_Canary.provisionprofile'
	: 'build_resources/profiles/Fluxer.provisionprofile';
const supportedTargetArchs = ['x64', 'arm64'];
const electronArch = process.env.ELECTRON_ARCH;
const cliTargetArch = supportedTargetArchs.find((arch) => process.argv.includes(`--${arch}`)) || null;
const targetNativeArch = electronArch || cliTargetArch;

if (electronArch && !supportedTargetArchs.includes(electronArch)) {
	throw new Error(`Unsupported ELECTRON_ARCH: ${electronArch}`);
}

const targetArchs = electronArch ? [electronArch] : supportedTargetArchs;
const winTargets = [
	{
		target: 'dir',
		arch: targetArchs,
	},
];
const fluxerNativePackages = [
	'@fluxer/mac-app-audio',
	'@fluxer/mac-clipboard',
	'@fluxer/mac-screen-capture',
	'@fluxer/mac-sysctl',
	'@fluxer/mac-tcc',
	'@fluxer/macos-input-hook',
	'@fluxer/win-process-loopback',
	...(windowsGameCaptureModuleEnabled ? ['@fluxer/win-game-capture'] : []),
	'@fluxer/win-clipboard',
	'@fluxer/win-shell',
	'@fluxer/win-toast',
	'@fluxer/windows-input-hook',
	'@fluxer/linux-audio-capture',
	'@fluxer/linux-portals',
	'@fluxer/linux-screen-capture',
	'@fluxer/linux-notifications',
	'@fluxer/linux-evdev',
	'@fluxer/linux-input-hook',
	'@fluxer/system-hunspell',
	'@fluxer/platform-info',
	'@fluxer/webauthn',
	'@fluxer/webrtc-sender',
];
const fluxerNativePackagesByPlatform = {
	darwin: [
		'@fluxer/mac-app-audio',
		'@fluxer/mac-clipboard',
		'@fluxer/mac-screen-capture',
		'@fluxer/mac-sysctl',
		'@fluxer/mac-tcc',
		'@fluxer/macos-input-hook',
		'@fluxer/platform-info',
		'@fluxer/webauthn',
		'@fluxer/webrtc-sender',
	],
	win32: [
		'@fluxer/win-process-loopback',
		...(windowsGameCaptureModuleEnabled ? ['@fluxer/win-game-capture'] : []),
		'@fluxer/win-clipboard',
		'@fluxer/win-shell',
		'@fluxer/win-toast',
		'@fluxer/windows-input-hook',
		'@fluxer/platform-info',
		'@fluxer/webauthn',
		'@fluxer/webrtc-sender',
	],
	linux: [
		'@fluxer/linux-audio-capture',
		'@fluxer/linux-portals',
		'@fluxer/linux-screen-capture',
		'@fluxer/linux-notifications',
		'@fluxer/linux-evdev',
		'@fluxer/linux-input-hook',
		'@fluxer/system-hunspell',
		'@fluxer/platform-info',
		'@fluxer/webauthn',
		'@fluxer/webrtc-sender',
	],
};
const velopackNativeFiles = [
	'velopack_nodeffi_linux_arm64_gnu.node',
	'velopack_nodeffi_linux_x64_gnu.node',
	'velopack_nodeffi_osx.node',
	'velopack_nodeffi_win_arm64_msvc.node',
	'velopack_nodeffi_win_x64_msvc.node',
	'velopack_nodeffi_win_x86_msvc.node',
];
const nativeRuntimeFilePatterns = [
	'node_modules/@fluxer/mac-app-audio/package.json',
	'node_modules/@fluxer/mac-app-audio/index.js',
	'node_modules/@fluxer/mac-app-audio/loader-diagnostics.cjs',
	'node_modules/@fluxer/mac-app-audio/*.node',
	'node_modules/@fluxer/mac-screen-capture/package.json',
	'node_modules/@fluxer/mac-screen-capture/index.js',
	'node_modules/@fluxer/mac-screen-capture/loader-diagnostics.cjs',
	'node_modules/@fluxer/mac-screen-capture/*.node',
	'node_modules/@fluxer/mac-clipboard/package.json',
	'node_modules/@fluxer/mac-clipboard/index.js',
	'node_modules/@fluxer/mac-clipboard/loader-diagnostics.cjs',
	'node_modules/@fluxer/mac-clipboard/*.node',
	'node_modules/@fluxer/mac-sysctl/package.json',
	'node_modules/@fluxer/mac-sysctl/index.js',
	'node_modules/@fluxer/mac-sysctl/loader-diagnostics.cjs',
	'node_modules/@fluxer/mac-sysctl/*.node',
	'node_modules/@fluxer/mac-tcc/package.json',
	'node_modules/@fluxer/mac-tcc/index.js',
	'node_modules/@fluxer/mac-tcc/loader-diagnostics.cjs',
	'node_modules/@fluxer/mac-tcc/*.node',
	'node_modules/@fluxer/win-process-loopback/package.json',
	'node_modules/@fluxer/win-process-loopback/index.js',
	'node_modules/@fluxer/win-process-loopback/binding.js',
	'node_modules/@fluxer/win-process-loopback/loader-diagnostics.cjs',
	'node_modules/@fluxer/win-process-loopback/*.node',
	...(windowsGameCaptureModuleEnabled
		? [
				'node_modules/@fluxer/win-game-capture/package.json',
				'node_modules/@fluxer/win-game-capture/index.js',
				'node_modules/@fluxer/win-game-capture/loader-diagnostics.cjs',
				'node_modules/@fluxer/win-game-capture/*.node',
				'node_modules/@fluxer/win-game-capture/*.dll',
				'node_modules/@fluxer/win-game-capture/*.exe',
				'node_modules/@fluxer/win-game-capture/compatibility.json',
				'node_modules/@fluxer/win-game-capture/fluxer-vulkan-layer.*.json',
			]
		: []),
	'node_modules/@fluxer/win-clipboard/package.json',
	'node_modules/@fluxer/win-clipboard/index.js',
	'node_modules/@fluxer/win-clipboard/loader-diagnostics.cjs',
	'node_modules/@fluxer/win-clipboard/*.node',
	'node_modules/@fluxer/win-shell/package.json',
	'node_modules/@fluxer/win-shell/index.js',
	'node_modules/@fluxer/win-shell/loader-diagnostics.cjs',
	'node_modules/@fluxer/win-shell/*.node',
	'node_modules/@fluxer/win-toast/package.json',
	'node_modules/@fluxer/win-toast/index.js',
	'node_modules/@fluxer/win-toast/loader-diagnostics.cjs',
	'node_modules/@fluxer/win-toast/*.node',
	'node_modules/@fluxer/linux-audio-capture/package.json',
	'node_modules/@fluxer/linux-audio-capture/index.js',
	'node_modules/@fluxer/linux-audio-capture/loader-diagnostics.cjs',
	'node_modules/@fluxer/linux-audio-capture/*.node',
	'node_modules/@fluxer/linux-portals/package.json',
	'node_modules/@fluxer/linux-portals/index.js',
	'node_modules/@fluxer/linux-portals/loader-diagnostics.cjs',
	'node_modules/@fluxer/linux-portals/*.node',
	'node_modules/@fluxer/linux-screen-capture/package.json',
	'node_modules/@fluxer/linux-screen-capture/index.js',
	'node_modules/@fluxer/linux-screen-capture/loader-diagnostics.cjs',
	'node_modules/@fluxer/linux-screen-capture/*.node',
	'node_modules/@fluxer/linux-screen-capture/THIRD_PARTY_OBS_VKCAPTURE.md',
	'node_modules/@fluxer/linux-screen-capture/obs-vkcapture/**/*',
	'node_modules/@fluxer/linux-notifications/package.json',
	'node_modules/@fluxer/linux-notifications/index.js',
	'node_modules/@fluxer/linux-notifications/loader-diagnostics.cjs',
	'node_modules/@fluxer/linux-notifications/*.node',
	'node_modules/@fluxer/linux-evdev/package.json',
	'node_modules/@fluxer/linux-evdev/index.js',
	'node_modules/@fluxer/linux-evdev/loader-diagnostics.cjs',
	'node_modules/@fluxer/linux-evdev/*.node',
	'node_modules/@fluxer/system-hunspell/package.json',
	'node_modules/@fluxer/system-hunspell/index.js',
	'node_modules/@fluxer/system-hunspell/loader-diagnostics.cjs',
	'node_modules/@fluxer/system-hunspell/*.node',
	'node_modules/@fluxer/macos-input-hook/package.json',
	'node_modules/@fluxer/macos-input-hook/index.js',
	'node_modules/@fluxer/macos-input-hook/loader-diagnostics.cjs',
	'node_modules/@fluxer/macos-input-hook/*.node',
	'node_modules/@fluxer/windows-input-hook/package.json',
	'node_modules/@fluxer/windows-input-hook/index.js',
	'node_modules/@fluxer/windows-input-hook/loader-diagnostics.cjs',
	'node_modules/@fluxer/windows-input-hook/*.node',
	'node_modules/@fluxer/linux-input-hook/package.json',
	'node_modules/@fluxer/linux-input-hook/index.js',
	'node_modules/@fluxer/linux-input-hook/loader-diagnostics.cjs',
	'node_modules/@fluxer/linux-input-hook/*.node',
	'node_modules/@fluxer/platform-info/package.json',
	'node_modules/@fluxer/platform-info/index.js',
	'node_modules/@fluxer/platform-info/loader-diagnostics.cjs',
	'node_modules/@fluxer/platform-info/*.node',
	'node_modules/@fluxer/webauthn/package.json',
	'node_modules/@fluxer/webauthn/index.js',
	'node_modules/@fluxer/webauthn/index.d.ts',
	'node_modules/@fluxer/webauthn/loader-diagnostics.cjs',
	'node_modules/@fluxer/webauthn/*.node',
	'node_modules/@fluxer/webauthn/*.so*',
	'node_modules/@fluxer/webrtc-sender/package.json',
	'node_modules/@fluxer/webrtc-sender/index.js',
	'node_modules/@fluxer/webrtc-sender/index.d.ts',
	'node_modules/@fluxer/webrtc-sender/*.node',
	'node_modules/.pnpm/@fluxer+*/node_modules/@fluxer/*/loader-diagnostics.cjs',
	'node_modules/.pnpm/@fluxer+win-process-loopback@*/node_modules/@fluxer/win-process-loopback/*.node',
	...(windowsGameCaptureModuleEnabled
		? [
				'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.node',
				'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.dll',
				'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.exe',
				'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/compatibility.json',
				'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/fluxer-vulkan-layer.*.json',
			]
		: []),
	'node_modules/.pnpm/@fluxer+win-clipboard@*/node_modules/@fluxer/win-clipboard/*.node',
	'node_modules/.pnpm/@fluxer+win-shell@*/node_modules/@fluxer/win-shell/*.node',
	'node_modules/.pnpm/@fluxer+win-toast@*/node_modules/@fluxer/win-toast/*.node',
	'node_modules/.pnpm/@fluxer+windows-input-hook@*/node_modules/@fluxer/windows-input-hook/*.node',
	'node_modules/.pnpm/@fluxer+linux-audio-capture@*/node_modules/@fluxer/linux-audio-capture/*.node',
	'node_modules/.pnpm/@fluxer+linux-portals@*/node_modules/@fluxer/linux-portals/*.node',
	'node_modules/.pnpm/@fluxer+linux-screen-capture@*/node_modules/@fluxer/linux-screen-capture/*.node',
	'node_modules/.pnpm/@fluxer+linux-screen-capture@*/node_modules/@fluxer/linux-screen-capture/THIRD_PARTY_OBS_VKCAPTURE.md',
	'node_modules/.pnpm/@fluxer+linux-screen-capture@*/node_modules/@fluxer/linux-screen-capture/obs-vkcapture/**/*',
	'node_modules/.pnpm/@fluxer+linux-notifications@*/node_modules/@fluxer/linux-notifications/*.node',
	'node_modules/.pnpm/@fluxer+linux-evdev@*/node_modules/@fluxer/linux-evdev/*.node',
	'node_modules/.pnpm/@fluxer+linux-input-hook@*/node_modules/@fluxer/linux-input-hook/*.node',
	'node_modules/.pnpm/@fluxer+mac-app-audio@*/node_modules/@fluxer/mac-app-audio/*.node',
	'node_modules/.pnpm/@fluxer+mac-screen-capture@*/node_modules/@fluxer/mac-screen-capture/*.node',
	'node_modules/.pnpm/@fluxer+mac-clipboard@*/node_modules/@fluxer/mac-clipboard/*.node',
	'node_modules/.pnpm/@fluxer+mac-sysctl@*/node_modules/@fluxer/mac-sysctl/*.node',
	'node_modules/.pnpm/@fluxer+mac-tcc@*/node_modules/@fluxer/mac-tcc/*.node',
	'node_modules/.pnpm/@fluxer+macos-input-hook@*/node_modules/@fluxer/macos-input-hook/*.node',
	'node_modules/.pnpm/@fluxer+platform-info@*/node_modules/@fluxer/platform-info/*.node',
	'node_modules/.pnpm/@fluxer+webauthn@*/node_modules/@fluxer/webauthn/*.node',
	'node_modules/.pnpm/@fluxer+webauthn@*/node_modules/@fluxer/webauthn/*.so*',
	'node_modules/.pnpm/@fluxer+webrtc-sender@*/node_modules/@fluxer/webrtc-sender/*.node',
];
const nativeBuildArtifactExcludes = [
	'!node_modules/@fluxer/**/src/**/*',
	'!node_modules/@fluxer/**/target/**/*',
	'!node_modules/@fluxer/**/build/**/*',
	'!node_modules/@fluxer/**/Cargo.toml',
	'!node_modules/@fluxer/**/Cargo.lock',
	'!node_modules/@fluxer/**/CMakeLists.txt',
	'!node_modules/@fluxer/**/tsconfig.json',
	'!node_modules/@fluxer/**/*.d.ts',
	'!node_modules/@fluxer/**/*.d.ts.map',
	'!node_modules/@fluxer/**/*.map',
	'!node_modules/@fluxer/**/*.rs',
	'!node_modules/@fluxer/**/*.swift',
];
const packagedRuntimeArtifactExcludes = [
	'!dist/**/*.map',
	'!node_modules/**/.cache/**/*',
	'!node_modules/**/.github/**/*',
	'!node_modules/**/.yarn/**/*',
	'!node_modules/**/.yarnrc.yml',
	'!node_modules/**/*.map',
	'!node_modules/**/*.d.ts',
	'!node_modules/**/*.d.ts.map',
	'!node_modules/**/*.tsbuildinfo',
	'!node_modules/**/tsconfig.json',
	'!node_modules/**/tsconfig.*.json',
	'!node_modules/**/README*',
	'!node_modules/**/CHANGELOG*',
	'!node_modules/hunspell-asm/src/**/*',
	'!node_modules/hunspell-asm/dist/esm/**/*',
	'!node_modules/hunspell-asm/dist/types/**/*',
	'!node_modules/hunspell-asm/dist/cjs/lib/browser/**/*',
	'!node_modules/emscripten-wasm-loader/src/**/*',
	'!node_modules/emscripten-wasm-loader/dist/esm/**/*',
	'!node_modules/emscripten-wasm-loader/dist/types/**/*',
];
const bundledDependencyExcludes = [
	'!node_modules/@homebridge/dbus-native/**/*',
	'!node_modules/@simplewebauthn/browser/**/*',
	'!node_modules/duplexer/**/*',
	'!node_modules/event-stream/**/*',
	'!node_modules/from/**/*',
	'!node_modules/hexy/**/*',
	'!node_modules/long/**/*',
	'!node_modules/map-stream/**/*',
	'!node_modules/minimist/**/*',
	'!node_modules/pause-stream/**/*',
	'!node_modules/safe-buffer/**/*',
	'!node_modules/sax/**/*',
	'!node_modules/split/**/*',
	'!node_modules/stream-combiner/**/*',
	'!node_modules/through/**/*',
	'!node_modules/xml2js/**/*',
	'!node_modules/xmlbuilder/**/*',
];
const windowsGameCapturePackageExcludes = !windowsGameCaptureModuleEnabled
	? ['!node_modules/@fluxer/win-game-capture/**/*', '!node_modules/.pnpm/@fluxer+win-game-capture@*/**/*']
	: [];
const platformNativeRuntimeExcludes = platformNativeExcludes(targetPlatform, targetNativeArch);
const platformRuntimeDependencyExcludes =
	targetPlatform === 'darwin'
		? []
		: ['!node_modules/github-url-to-object/**/*', '!node_modules/ms/**/*', '!node_modules/update-electron-app/**/*'];
const linuxDesktopEntry = {
	Name: productName,
	GenericName: 'Instant Messenger',
	Comment: isCanary ? 'Canary build of Fluxer' : 'Instant messaging and VoIP',
	Keywords: 'chat;im;messaging;messenger;voip;voice;video;call;',
	Categories: 'Network;InstantMessaging;Chat;',
	StartupWMClass: linuxPackageName,
	StartupNotify: 'true',
	SingleMainWindow: 'true',
	MimeType: 'x-scheme-handler/fluxer;',
	'X-GNOME-UsesNotifications': 'true',
};
const linuxDesktopEntryWithActions = {
	...linuxDesktopEntry,
	Actions: linuxDesktopActionList,
};
const linuxInstalledExecPath = quoteDesktopExecArg(path.posix.join('/opt', productName, linuxPackageName));
const linuxDesktopActions = {
	'open-settings': {
		Name: 'Open Settings',
		Exec: buildLinuxDesktopTaskExec('open-settings'),
	},
	'new-dm': {
		Name: 'New Direct Message',
		Exec: buildLinuxDesktopTaskExec('new-dm'),
	},
};

function velopackNativeFile(platform, arch) {
	if (platform === 'darwin') return 'velopack_nodeffi_osx.node';
	if (!arch) return null;
	if (platform === 'win32') return `velopack_nodeffi_win_${arch}_msvc.node`;
	if (platform === 'linux') return `velopack_nodeffi_linux_${arch}_gnu.node`;
	return null;
}

function platformNativeExcludes(platform, arch) {
	if (!arch) return [];
	const keepFluxerPackages = new Set(fluxerNativePackagesByPlatform[platform] ?? []);
	const keepVelopackNativeFile = velopackNativeFile(platform, arch);
	return [
		...fluxerNativePackages
			.filter((packageName) => !keepFluxerPackages.has(packageName))
			.map((packageName) => `!node_modules/${packageName}/**/*`),
		...(platform === 'win32'
			? velopackNativeFiles
					.filter((fileName) => fileName !== keepVelopackNativeFile)
					.map((fileName) => `!node_modules/velopack/lib/native/${fileName}`)
			: ['!node_modules/velopack/**/*']),
	];
}

function quoteDesktopExecArg(value) {
	return `"${value.replace(/(["`$\\])/g, '\\$1')}"`;
}

function buildLinuxDesktopTaskExec(taskId) {
	return `${linuxInstalledExecPath} --fluxer-task=${taskId} %U`;
}

function normalizeArch(arch) {
	if (arch === 'x64' || arch === 'arm64') {
		return arch;
	}
	if (arch === 1) return 'x64';
	if (arch === 3) return 'arm64';
	return electronArch || process.arch;
}

function platformTag(platform, arch) {
	if (platform === 'darwin') return `darwin-${arch}`;
	if (platform === 'win32') return `win32-${arch}-msvc`;
	if (platform === 'linux') return `linux-${arch}-gnu`;
	return null;
}

function addWindowsGameCaptureArtifacts(artifacts, tag, arch) {
	if (!windowsGameCaptureModuleEnabled) return;
	const add = (relativePath) => {
		artifacts.push({
			packageName: '@fluxer/win-game-capture',
			relativePath,
		});
	};
	add(`win-game-capture.${tag}.node`);
	add(`fluxer-game-hook.${tag}.dll`);
	add(`fluxer-inject-helper.${tag}.exe`);
	add(`fluxer-vulkan-layer.${tag}.dll`);
	add(`fluxer-vulkan-layer.${tag}.json`);
	if (arch === 'x64') {
		add('fluxer-game-hook.win32-ia32-msvc.dll');
		add('fluxer-inject-helper.win32-ia32-msvc.exe');
	}
}

function expectedNativeRuntimeArtifacts(platform, arch) {
	const tag = platformTag(platform, arch);
	if (!tag) return [];
	const artifacts = [];
	artifacts.push({
		packageName: '@fluxer/webauthn',
		relativePath: `webauthn.${tag}.node`,
	});
	artifacts.push({
		packageName: '@fluxer/webrtc-sender',
		relativePath: `webrtc-sender.${tag}.node`,
	});
	if (platform === 'darwin') {
		artifacts.push({
			packageName: '@fluxer/mac-app-audio',
			relativePath: `mac-app-audio.darwin-${arch}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/mac-screen-capture',
			relativePath: `mac-screen-capture.darwin-${arch}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/mac-clipboard',
			relativePath: `mac-clipboard.darwin-${arch}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/mac-sysctl',
			relativePath: `mac-sysctl.darwin-${arch}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/mac-tcc',
			relativePath: `mac-tcc.darwin-${arch}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/macos-input-hook',
			relativePath: `macos-input-hook.darwin-${arch}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/platform-info',
			relativePath: `platform-info.${tag}.node`,
		});
	} else if (platform === 'win32') {
		artifacts.push({
			packageName: '@fluxer/win-process-loopback',
			relativePath: `win-process-loopback.${tag}.node`,
		});
		addWindowsGameCaptureArtifacts(artifacts, tag, arch);
		artifacts.push({
			packageName: '@fluxer/win-clipboard',
			relativePath: `win-clipboard.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/win-shell',
			relativePath: `win-shell.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/win-toast',
			relativePath: `win-toast.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/windows-input-hook',
			relativePath: `windows-input-hook.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/platform-info',
			relativePath: `platform-info.${tag}.node`,
		});
	} else if (platform === 'linux') {
		artifacts.push({
			packageName: '@fluxer/linux-audio-capture',
			relativePath: `linux-audio-capture.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/linux-portals',
			relativePath: `linux-portals.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/linux-screen-capture',
			relativePath: `linux-screen-capture.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/linux-notifications',
			relativePath: `linux-notifications.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/linux-evdev',
			relativePath: `linux-evdev.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/system-hunspell',
			relativePath: `system-hunspell.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/linux-input-hook',
			relativePath: `linux-input-hook.${tag}.node`,
		});
		artifacts.push({
			packageName: '@fluxer/platform-info',
			relativePath: `platform-info.${tag}.node`,
		});
	}
	return artifacts;
}

function isLinuxSharedLibraryArtifact(fileName) {
	return /\.so(?:\.|$)/.test(fileName);
}

async function expectedNativeRuntimeArtifactsForAppDir(platform, arch, appDir) {
	const artifacts = expectedNativeRuntimeArtifacts(platform, arch);
	if (platform !== 'linux') return artifacts;
	const webAuthnRoot = path.join(appDir, 'node_modules', '@fluxer', 'webauthn');
	const linuxWebAuthnRuntimeLibraries = new Set(['libfido2.so.1']);
	try {
		for (const entry of await fs.readdir(webAuthnRoot, {withFileTypes: true})) {
			if (entry.isFile() && isLinuxSharedLibraryArtifact(entry.name)) {
				linuxWebAuthnRuntimeLibraries.add(entry.name);
			}
		}
	} catch (error) {
		if (!error || error.code !== 'ENOENT') throw error;
	}
	for (const libraryName of [...linuxWebAuthnRuntimeLibraries].sort()) {
		artifacts.push({
			packageName: '@fluxer/webauthn',
			relativePath: libraryName,
		});
	}
	return artifacts;
}

function packagePathParts(packageName) {
	const parts = packageName.split('/');
	if (parts.length === 2 && parts[0].startsWith('@')) {
		return parts;
	}
	return [packageName];
}

function isNonEmptyString(value) {
	return typeof value === 'string' && value.length > 0;
}

function resolveAppDir(context) {
	const candidates = [
		context.appDir,
		context.packager?.info?.appDir,
		context.packager?.appDir,
		context.packager?.projectDir,
		context.packager?.info?.projectDir,
		process.cwd(),
	];
	const appDir = candidates.find(isNonEmptyString);
	if (!appDir) {
		throw new Error('Unable to resolve electron-builder app directory for native artifact verification.');
	}
	return appDir;
}

async function fileExists(filePath) {
	return fs
		.stat(filePath)
		.then((stat) => stat.isFile())
		.catch((error) => {
			if (error && error.code === 'ENOENT') {
				return false;
			}
			throw error;
		});
}

function expectedDarwinMachOArch(arch) {
	if (arch === 'x64') return 'x86_64';
	if (arch === 'arm64') return 'arm64';
	return null;
}

async function darwinMachOArchitectures(filePath) {
	const {stdout} = await execFileAsync('lipo', ['-archs', filePath]);
	return stdout.trim().split(/\s+/).filter(Boolean);
}

async function darwinMachOFileTypes(filePath) {
	const {stdout} = await execFileAsync('otool', ['-hv', filePath]);
	const knownFileTypes = new Set(['OBJECT', 'EXECUTE', 'FVMLIB', 'CORE', 'PRELOAD', 'DYLIB', 'DYLINKER', 'BUNDLE']);
	return stdout
		.split(/\r?\n/)
		.flatMap((line) => line.trim().split(/\s+/))
		.filter((token) => knownFileTypes.has(token));
}

async function darwinMachOLoadCommands(filePath) {
	const {stdout} = await execFileAsync('otool', ['-l', filePath]);
	return stdout
		.split(/\r?\n/)
		.map((line) => line.trim().match(/^cmd\s+(\S+)$/)?.[1])
		.filter(Boolean);
}

async function verifyDarwinNativeArchitectures(platform, arch, entries, stage) {
	if (platform !== 'darwin') return;
	const expectedArch = expectedDarwinMachOArch(arch);
	if (!expectedArch) return;
	const mismatches = [];
	for (const entry of entries) {
		if (!(await fileExists(entry.path))) continue;
		const archs = await darwinMachOArchitectures(entry.path);
		const fileTypes = await darwinMachOFileTypes(entry.path);
		if (!archs.includes(expectedArch)) {
			mismatches.push(`${entry.label}: has ${archs.join(', ') || '<none>'}; expected ${expectedArch}`);
		}
		if (arch === 'x64' && archs.includes('x86_64h') && !archs.includes('x86_64')) {
			mismatches.push(`${entry.label}: has x86_64h only; expected baseline x86_64 for Intel compatibility`);
		}
		if (fileTypes.length === 0 || fileTypes.some((fileType) => fileType !== 'BUNDLE' && fileType !== 'DYLIB')) {
			mismatches.push(
				`${entry.label}: Mach-O file type ${fileTypes.join(', ') || '<none>'}; expected DYLIB or BUNDLE for Node addon loading`,
			);
		}
		if (fileTypes.includes('BUNDLE')) {
			const loadCommands = await darwinMachOLoadCommands(entry.path);
			if (loadCommands.includes('LC_ID_DYLIB')) {
				mismatches.push(
					`${entry.label}: Mach-O BUNDLE contains LC_ID_DYLIB; dyld rejects this combination when Electron loads the addon`,
				);
			}
		}
	}
	if (mismatches.length > 0) {
		throw new Error(
			[
				`Wrong native runtime architecture(s) ${stage} for ${platform}/${arch}:`,
				...mismatches.map((entry) => `  - ${entry}`),
			].join('\n'),
		);
	}
}

async function verifyNativePackageInputs(context) {
	if (process.env.FLUXER_SKIP_NATIVE === 'true') return;
	const platform = context.electronPlatformName;
	const arch = normalizeArch(context.arch);
	const appDir = resolveAppDir(context);
	const missing = [];
	const entries = [];
	for (const artifact of await expectedNativeRuntimeArtifactsForAppDir(platform, arch, appDir)) {
		const artifactPath = path.join(
			appDir,
			'node_modules',
			...packagePathParts(artifact.packageName),
			artifact.relativePath,
		);
		entries.push({
			label: `${artifact.packageName}/${artifact.relativePath}`,
			path: artifactPath,
		});
		if (!(await fileExists(artifactPath))) {
			missing.push(`${artifact.packageName}/${artifact.relativePath}`);
		}
	}
	if (missing.length > 0) {
		throw new Error(
			[
				`Missing native runtime artifact(s) before packaging for ${platform}/${arch}:`,
				...missing.map((entry) => `  - ${entry}`),
				'Run `pnpm build` from fluxer_desktop so native build outputs are synced into node_modules before electron-builder runs.',
			].join('\n'),
		);
	}
	await verifyDarwinNativeArchitectures(platform, arch, entries, 'before packaging');
}

async function verifyPackagedNativeArtifacts(context) {
	const platform = context.electronPlatformName;
	const arch = normalizeArch(context.arch);
	const appDir = resolveAppDir(context);
	const missing = [];
	const entries = [];
	for (const artifact of await expectedNativeRuntimeArtifactsForAppDir(platform, arch, appDir)) {
		const artifactPath = path.join(
			context.appOutDir,
			'resources',
			'app.asar.unpacked',
			'node_modules',
			...packagePathParts(artifact.packageName),
			artifact.relativePath,
		);
		entries.push({
			label: `${artifact.packageName}/${artifact.relativePath}`,
			path: artifactPath,
		});
		if (!(await fileExists(artifactPath))) {
			missing.push(`${artifact.packageName}/${artifact.relativePath}`);
		}
	}
	if (missing.length > 0) {
		throw new Error(
			[
				`Missing unpacked native runtime artifact(s) after packaging for ${platform}/${arch}:`,
				...missing.map((entry) => `  - ${entry}`),
				'Check electron-builder asarUnpack patterns and native package artifact sync.',
			].join('\n'),
		);
	}
	await verifyDarwinNativeArchitectures(platform, arch, entries, 'after packaging');
}

async function copyMissingPackagedNativeArtifacts(context) {
	const platform = context.electronPlatformName;
	const arch = normalizeArch(context.arch);
	const appDir = resolveAppDir(context);
	for (const artifact of await expectedNativeRuntimeArtifactsForAppDir(platform, arch, appDir)) {
		const packageParts = packagePathParts(artifact.packageName);
		const sourcePath = path.join(appDir, 'node_modules', ...packageParts, artifact.relativePath);
		const targetPath = path.join(
			context.appOutDir,
			'resources',
			'app.asar.unpacked',
			'node_modules',
			...packageParts,
			artifact.relativePath,
		);
		if ((await fileExists(targetPath)) || !(await fileExists(sourcePath))) {
			continue;
		}
		await fs.mkdir(path.dirname(targetPath), {recursive: true});
		await fs.copyFile(sourcePath, targetPath);
	}
}

async function removeIfExists(targetPath) {
	await fs.rm(targetPath, {force: true, recursive: true});
}

async function cleanupNativeBuildIntermediates(context) {
	const unpackedNodeModules = path.join(context.appOutDir, 'resources', 'app.asar.unpacked', 'node_modules');
	const entries = await fs.readdir(unpackedNodeModules, {withFileTypes: true}).catch((error) => {
		if (error && error.code === 'ENOENT') {
			return [];
		}
		throw error;
	});
	const packageDirs = [];
	for (const entry of entries) {
		if (!entry.isDirectory()) {
			continue;
		}
		const entryPath = path.join(unpackedNodeModules, entry.name);
		if (entry.name.startsWith('@')) {
			const scopedEntries = await fs.readdir(entryPath, {withFileTypes: true});
			for (const scopedEntry of scopedEntries) {
				if (scopedEntry.isDirectory()) {
					packageDirs.push(path.join(entryPath, scopedEntry.name));
				}
			}
		} else {
			packageDirs.push(entryPath);
		}
	}
	await Promise.all(packageDirs.map((packageDir) => removeIfExists(path.join(packageDir, 'build', 'Release', 'obj'))));
}

async function addLinuxLegacyBinarySymlink(context) {
	if (context.electronPlatformName !== 'linux') return;
	const legacyName = packageName;
	const currentName = linuxPackageName;
	if (legacyName === currentName) return;
	const linkPath = path.join(context.appOutDir, legacyName);
	try {
		await fs.symlink(currentName, linkPath);
	} catch (error) {
		if (!error || error.code !== 'EEXIST') throw error;
	}
}

async function afterPack(context) {
	await copyMissingPackagedNativeArtifacts(context);
	await cleanupNativeBuildIntermediates(context);
	await addLinuxLegacyBinarySymlink(context);
	await verifyPackagedNativeArtifacts(context);
}

async function listRpmPackageFiles(artifactPath) {
	try {
		const {stdout} = await execFileAsync('rpm', ['-qpl', artifactPath], {
			maxBuffer: 16 * 1024 * 1024,
		});
		return stdout
			.split(/\r?\n/)
			.map((line) => line.trim())
			.filter(Boolean);
	} catch (error) {
		if (error && error.code === 'ENOENT') {
			throw new Error(`Cannot inspect RPM artifact ${artifactPath}: rpm executable is not available.`);
		}
		const stderr = typeof error?.stderr === 'string' ? error.stderr.trim() : '';
		throw new Error(`Cannot inspect RPM artifact ${artifactPath}: ${stderr || error?.message || String(error)}`);
	}
}

async function listDebPackageFiles(artifactPath) {
	try {
		const {stdout} = await execFileAsync('dpkg-deb', ['--contents', artifactPath], {
			maxBuffer: 16 * 1024 * 1024,
		});
		return stdout
			.split(/\r?\n/)
			.map((line) => line.match(/^\S+\s+\S+\s+\d+\s+\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}\s+(.+)$/)?.[1])
			.filter(Boolean)
			.map((filePath) => filePath.replace(/^\.\//, '/'));
	} catch (error) {
		if (error && error.code === 'ENOENT') {
			throw new Error(`Cannot inspect DEB artifact ${artifactPath}: dpkg-deb executable is not available.`);
		}
		const stderr = typeof error?.stderr === 'string' ? error.stderr.trim() : '';
		throw new Error(`Cannot inspect DEB artifact ${artifactPath}: ${stderr || error?.message || String(error)}`);
	}
}

async function verifyRpmArtifactsDoNotOwnBuildIds(buildResult) {
	const rpmArtifacts = (buildResult.artifactPaths ?? []).filter(
		(artifactPath) => path.extname(artifactPath) === '.rpm',
	);
	const violations = [];
	for (const artifactPath of rpmArtifacts) {
		const packageFiles = await listRpmPackageFiles(artifactPath);
		const buildIdFiles = packageFiles.filter(
			(filePath) => filePath === rpmBuildIdFilePrefix || filePath.startsWith(`${rpmBuildIdFilePrefix}/`),
		);
		if (buildIdFiles.length > 0) {
			violations.push({artifactPath, buildIdFiles});
		}
	}
	if (violations.length === 0) return [];

	const lines = [
		`RPM artifact(s) must not own ${rpmBuildIdFilePrefix} entries.`,
		'These global rpmbuild-generated links collide with other Electron RPMs that bundle the same upstream ELF binaries.',
	];
	for (const {artifactPath, buildIdFiles} of violations) {
		lines.push(`  - ${path.basename(artifactPath)}:`);
		for (const filePath of buildIdFiles.slice(0, 12)) {
			lines.push(`    ${filePath}`);
		}
		if (buildIdFiles.length > 12) {
			lines.push(`    ... ${buildIdFiles.length - 12} more`);
		}
	}
	throw new Error(lines.join('\n'));
}

function packageFilesContainAppArmorProfile(packageFiles) {
	return packageFiles.some((filePath) => filePath.endsWith('/resources/apparmor-profile'));
}

async function verifyLinuxPackagesContainAppArmorProfile(buildResult) {
	const packageArtifacts = (buildResult.artifactPaths ?? []).filter((artifactPath) =>
		['.deb', '.rpm'].includes(path.extname(artifactPath)),
	);
	const violations = [];
	for (const artifactPath of packageArtifacts) {
		const extension = path.extname(artifactPath);
		const packageFiles =
			extension === '.deb' ? await listDebPackageFiles(artifactPath) : await listRpmPackageFiles(artifactPath);
		if (!packageFilesContainAppArmorProfile(packageFiles)) {
			violations.push({artifactPath});
		}
	}
	if (violations.length === 0) return;

	const lines = [
		'Linux package artifact(s) must include the Electron AppArmor profile.',
		'Ubuntu 24.04+ restricts unprivileged user namespaces; packaged Electron apps need this profile so the Chromium sandbox can run without forcing --no-sandbox.',
	];
	for (const {artifactPath} of violations) {
		lines.push(`  - ${path.basename(artifactPath)}`);
	}
	throw new Error(lines.join('\n'));
}

async function readElfNeededLibraries(artifactPath) {
	try {
		const {stdout} = await execFileAsync('readelf', ['-d', artifactPath], {
			maxBuffer: 8 * 1024 * 1024,
		});
		return stdout
			.split(/\r?\n/)
			.map((line) => line.match(/\(NEEDED\)\s+Shared library: \[([^\]]+)\]/)?.[1])
			.filter(Boolean);
	} catch (error) {
		if (error && error.code === 'ENOENT') {
			throw new Error(`Cannot inspect AppImage artifact ${artifactPath}: readelf executable is not available.`);
		}
		const stderr = typeof error?.stderr === 'string' ? error.stderr.trim() : '';
		throw new Error(`Cannot inspect AppImage artifact ${artifactPath}: ${stderr || error?.message || String(error)}`);
	}
}

async function verifyAppImageArtifactsDoNotNeedFuse2(buildResult) {
	const appImageArtifacts = (buildResult.artifactPaths ?? []).filter(
		(artifactPath) => path.extname(artifactPath) === '.AppImage',
	);
	const violations = [];
	for (const artifactPath of appImageArtifacts) {
		const neededLibraries = await readElfNeededLibraries(artifactPath);
		if (neededLibraries.includes('libfuse.so.2')) {
			violations.push({artifactPath, neededLibraries});
		}
	}
	if (violations.length === 0) return;

	const lines = [
		'AppImage artifact(s) must not depend on libfuse.so.2.',
		'Use electron-builder toolsets.appimage 1.0.3 so the AppImage runtime is static and works on modern distributions without libfuse2.',
	];
	for (const {artifactPath, neededLibraries} of violations) {
		lines.push(`  - ${path.basename(artifactPath)} imports: ${neededLibraries.join(', ') || '<none>'}`);
	}
	throw new Error(lines.join('\n'));
}

function canExecuteAppImageArtifact(artifactPath) {
	const artifactName = path.basename(artifactPath).toLowerCase();
	const hostArch = process.arch;
	if (artifactName.includes('arm64') || artifactName.includes('aarch64')) {
		return hostArch === 'arm64';
	}
	if (artifactName.includes('x86_64') || artifactName.includes('amd64') || artifactName.includes('x64')) {
		return hostArch === 'x64';
	}
	return true;
}

async function extractAppImagePattern(artifactPath, pattern, tempDir) {
	try {
		await execFileAsync(path.resolve(artifactPath), ['--appimage-extract', pattern], {
			cwd: tempDir,
			maxBuffer: 8 * 1024 * 1024,
		});
	} catch (error) {
		const stderr = typeof error?.stderr === 'string' ? error.stderr.trim() : '';
		throw new Error(
			`Cannot extract ${pattern} from AppImage artifact ${artifactPath}: ${stderr || error?.message || String(error)}`,
		);
	}
}

async function findExtractedFiles(rootDir, predicate) {
	const results = [];
	async function visit(directory) {
		const entries = await fs.readdir(directory, {withFileTypes: true});
		for (const entry of entries) {
			const entryPath = path.join(directory, entry.name);
			if (entry.isDirectory()) {
				await visit(entryPath);
			} else if (entry.isFile() && predicate(entryPath)) {
				results.push(entryPath);
			}
		}
	}
	await visit(rootDir);
	return results;
}

async function inspectAppImageLauncher(artifactPath) {
	const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'fluxer-appimage-'));
	try {
		await extractAppImagePattern(artifactPath, 'AppRun', tempDir);
		await extractAppImagePattern(artifactPath, '*.desktop', tempDir);
		const squashfsRoot = path.join(tempDir, 'squashfs-root');
		const appRun = await fs.readFile(path.join(squashfsRoot, 'AppRun'), 'utf8');
		const desktopFiles = await findExtractedFiles(squashfsRoot, (filePath) => path.extname(filePath) === '.desktop');
		const violations = [];

		if (desktopFiles.length === 0) {
			violations.push('does not contain a desktop entry');
		}

		const appRunUsesNamespaceProbe = /unshare\s+(?:-Ur|--user)\s+true/.test(appRun);
		if (!appRunUsesNamespaceProbe || !appRun.includes('NO_SANDBOX=--no-sandbox')) {
			violations.push('AppRun does not use the expected user-namespace probe before falling back to --no-sandbox');
		}

		for (const desktopFile of desktopFiles) {
			const desktopEntry = await fs.readFile(desktopFile, 'utf8');
			for (const line of desktopEntry.split(/\r?\n/)) {
				if (line.startsWith('Exec=') && line.includes('--no-sandbox')) {
					violations.push(`${path.basename(desktopFile)} has an unconditional --no-sandbox Exec line`);
				}
			}
		}

		return violations;
	} finally {
		await fs.rm(tempDir, {recursive: true, force: true});
	}
}

async function verifyAppImageArtifactsUseSandboxAwareLauncher(buildResult) {
	const appImageArtifacts = (buildResult.artifactPaths ?? []).filter(
		(artifactPath) => path.extname(artifactPath) === '.AppImage',
	);
	const violations = [];
	for (const artifactPath of appImageArtifacts) {
		if (!canExecuteAppImageArtifact(artifactPath)) {
			console.warn(
				`Skipping AppImage launcher extraction for ${path.basename(artifactPath)} because it does not match host architecture ${process.arch}.`,
			);
			continue;
		}
		const artifactViolations = await inspectAppImageLauncher(artifactPath);
		if (artifactViolations.length > 0) {
			violations.push({artifactPath, artifactViolations});
		}
	}
	if (violations.length === 0) return;

	const lines = [
		'AppImage artifact(s) must use the static-runtime, sandbox-aware launcher contract.',
		'The desktop entry must not pass --no-sandbox unconditionally; AppRun may add it only after the user-namespace probe fails.',
	];
	for (const {artifactPath, artifactViolations} of violations) {
		lines.push(`  - ${path.basename(artifactPath)}:`);
		for (const violation of artifactViolations) {
			lines.push(`    ${violation}`);
		}
	}
	throw new Error(lines.join('\n'));
}

async function verifyLinuxArtifactContracts(buildResult) {
	await verifyRpmArtifactsDoNotOwnBuildIds(buildResult);
	await verifyLinuxPackagesContainAppArmorProfile(buildResult);
	await verifyAppImageArtifactsDoNotNeedFuse2(buildResult);
	await verifyAppImageArtifactsUseSandboxAwareLauncher(buildResult);
}

module.exports = {
	appId,
	productName,
	copyright: 'Copyright © 2026 Fluxer Platform AB',
	// biome-ignore lint/suspicious/noTemplateCurlyInString: electron-builder placeholders, not JS template literals.
	artifactName: '${productName}-${version}-${os}-${arch}.${ext}',
	directories: {
		buildResources: 'build_resources',
		output: 'dist-electron',
	},
	files: [
		'dist/**/*',
		'package.json',
		...nativeRuntimeFilePatterns,
		'node_modules/hunspell-asm/**/*',
		...nativeBuildArtifactExcludes,
		...packagedRuntimeArtifactExcludes,
		...bundledDependencyExcludes,
		...windowsGameCapturePackageExcludes,
		...platformNativeRuntimeExcludes,
		...platformRuntimeDependencyExcludes,
	],
	extraMetadata: {
		main: 'dist/main/index.js',
		name: metadataName,
		...(Boolean(process.env.VERSION) ? {version: process.env.VERSION} : {}),
		...(targetPlatform === 'linux' ? {desktopName: `${linuxPackageName}.desktop`} : {}),
	},
	extraResources: [
		{
			from: `build_resources/${iconDir}/`,
			to: 'icons',
			filter: [
				'16x16.png',
				'24x24.png',
				'32x32.png',
				'48x48.png',
				'64x64.png',
				'128x128.png',
				'256x256.png',
				'512x512.png',
				'FluxerTrayTemplate.png',
				'FluxerTrayTemplate@2x.png',
				'icon.ico',
				'icon.png',
			],
		},
		{
			from: `build_resources/${iconDir}/badges/`,
			to: 'badges',
			filter: ['**/*'],
		},
	],
	asar: {
		smartUnpack: false,
	},
	asarUnpack: [
		'**/*.node',
		'node_modules/@fluxer/win-process-loopback/*.node',
		...(windowsGameCaptureModuleEnabled
			? [
					'node_modules/@fluxer/win-game-capture/*.node',
					'node_modules/@fluxer/win-game-capture/*.dll',
					'node_modules/@fluxer/win-game-capture/*.exe',
					'node_modules/@fluxer/win-game-capture/*.json',
				]
			: []),
		'node_modules/@fluxer/win-clipboard/*.node',
		'node_modules/@fluxer/win-shell/*.node',
		'node_modules/@fluxer/win-toast/*.node',
		'node_modules/@fluxer/linux-audio-capture/*.node',
		'node_modules/@fluxer/linux-portals/*.node',
		'node_modules/@fluxer/linux-screen-capture/*.node',
		'node_modules/@fluxer/linux-screen-capture/obs-vkcapture/**/*',
		'node_modules/@fluxer/linux-notifications/*.node',
		'node_modules/@fluxer/linux-evdev/*.node',
		'node_modules/@fluxer/system-hunspell/*.node',
		'node_modules/@fluxer/macos-input-hook/*.node',
		'node_modules/@fluxer/mac-app-audio/*.node',
		'node_modules/@fluxer/mac-screen-capture/*.node',
		'node_modules/@fluxer/mac-clipboard/*.node',
		'node_modules/@fluxer/mac-sysctl/*.node',
		'node_modules/@fluxer/mac-tcc/*.node',
		'node_modules/@fluxer/windows-input-hook/*.node',
		'node_modules/@fluxer/linux-input-hook/*.node',
		'node_modules/@fluxer/platform-info/*.node',
		'node_modules/@fluxer/webauthn/*.node',
		'node_modules/@fluxer/webauthn/*.so*',
		'node_modules/.pnpm/@fluxer+win-process-loopback@*/node_modules/@fluxer/win-process-loopback/*.node',
		...(windowsGameCaptureModuleEnabled
			? [
					'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.node',
					'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.dll',
					'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.exe',
					'node_modules/.pnpm/@fluxer+win-game-capture@*/node_modules/@fluxer/win-game-capture/*.json',
				]
			: []),
		'node_modules/.pnpm/@fluxer+win-clipboard@*/node_modules/@fluxer/win-clipboard/*.node',
		'node_modules/.pnpm/@fluxer+win-shell@*/node_modules/@fluxer/win-shell/*.node',
		'node_modules/.pnpm/@fluxer+win-toast@*/node_modules/@fluxer/win-toast/*.node',
		'node_modules/.pnpm/@fluxer+windows-input-hook@*/node_modules/@fluxer/windows-input-hook/*.node',
		'node_modules/.pnpm/@fluxer+linux-audio-capture@*/node_modules/@fluxer/linux-audio-capture/*.node',
		'node_modules/.pnpm/@fluxer+linux-portals@*/node_modules/@fluxer/linux-portals/*.node',
		'node_modules/.pnpm/@fluxer+linux-screen-capture@*/node_modules/@fluxer/linux-screen-capture/*.node',
		'node_modules/.pnpm/@fluxer+linux-screen-capture@*/node_modules/@fluxer/linux-screen-capture/obs-vkcapture/**/*',
		'node_modules/.pnpm/@fluxer+linux-notifications@*/node_modules/@fluxer/linux-notifications/*.node',
		'node_modules/.pnpm/@fluxer+linux-evdev@*/node_modules/@fluxer/linux-evdev/*.node',
		'node_modules/.pnpm/@fluxer+linux-input-hook@*/node_modules/@fluxer/linux-input-hook/*.node',
		'node_modules/.pnpm/@fluxer+system-hunspell@*/node_modules/@fluxer/system-hunspell/*.node',
		'node_modules/.pnpm/@fluxer+macos-input-hook@*/node_modules/@fluxer/macos-input-hook/*.node',
		'node_modules/.pnpm/@fluxer+mac-app-audio@*/node_modules/@fluxer/mac-app-audio/*.node',
		'node_modules/.pnpm/@fluxer+mac-screen-capture@*/node_modules/@fluxer/mac-screen-capture/*.node',
		'node_modules/.pnpm/@fluxer+mac-clipboard@*/node_modules/@fluxer/mac-clipboard/*.node',
		'node_modules/.pnpm/@fluxer+mac-sysctl@*/node_modules/@fluxer/mac-sysctl/*.node',
		'node_modules/.pnpm/@fluxer+mac-tcc@*/node_modules/@fluxer/mac-tcc/*.node',
		'node_modules/.pnpm/@fluxer+platform-info@*/node_modules/@fluxer/platform-info/*.node',
		'node_modules/.pnpm/@fluxer+webauthn@*/node_modules/@fluxer/webauthn/*.node',
		'node_modules/.pnpm/@fluxer+webauthn@*/node_modules/@fluxer/webauthn/*.so*',
	],
	compression: 'normal',
	npmRebuild: false,
	protocols: [
		{
			name: appId,
			role: 'Viewer',
			schemes: ['fluxer'],
		},
	],
	beforePack: verifyNativePackageInputs,
	afterPack,
	afterAllArtifactBuild: verifyLinuxArtifactContracts,
	toolsets: {
		appimage: '1.0.3',
	},
	mac: {
		category: 'public.app-category.social-networking',
		minimumSystemVersion: macOSMinimumSystemVersion,
		icon: `build_resources/${iconDir}/_compiled/AppIcon.icns`,
		darkModeSupport: true,
		hardenedRuntime: true,
		gatekeeperAssess: false,
		notarize: true,
		provisioningProfile,
		entitlements: isCanary
			? 'build_resources/entitlements.mac.canary.plist'
			: 'build_resources/entitlements.mac.stable.plist',
		entitlementsInherit: 'build_resources/entitlements.mac.inherit.plist',
		target: [
			{
				target: 'dmg',
				arch: targetArchs,
			},
			{
				target: 'zip',
				arch: targetArchs,
			},
		],
		extendInfo: {
			NSMicrophoneUsageDescription: 'Fluxer needs access to your microphone to enable voice chat features.',
			NSCameraUsageDescription: 'Fluxer needs access to your camera to enable video chat features.',
			NSAppleEventsUsageDescription: 'Fluxer needs access to Apple Events for automation features.',
			NSAudioCaptureUsageDescription: 'Fluxer captures audio from the screen or window you choose to share.',
			NSScreenCaptureUsageDescription: 'Fluxer captures the screen or window you choose to share.',
		},
	},
	dmg: {
		contents: [
			{
				x: 130,
				y: 220,
			},
			{
				x: 410,
				y: 220,
				type: 'link',
				path: '/Applications',
			},
		],
	},
	win: {
		icon: `build_resources/${iconDir}/icon.ico`,
		target: winTargets,
	},
	portable: {
		// biome-ignore lint/suspicious/noTemplateCurlyInString: electron-builder expands these placeholders.
		artifactName: '${productName}-${version}-portable-${os}-${arch}.${ext}',
	},
	linux: {
		icon: `build_resources/${iconDir}/icon.png`,
		category: 'Network;InstantMessaging;Chat;',
		target: [
			{
				target: 'AppImage',
				arch: targetArchs,
			},
			{
				target: 'deb',
				arch: targetArchs,
			},
			{
				target: 'rpm',
				arch: targetArchs,
			},
			{
				target: 'tar.gz',
				arch: targetArchs,
			},
		],
		desktop: {
			entry: linuxDesktopEntry,
		},
	},
	deb: {
		packageCategory: 'net',
		desktop: {
			entry: linuxDesktopEntryWithActions,
			desktopActions: linuxDesktopActions,
		},
		depends: [
			'libgtk-3-0',
			'libnotify4',
			'libnss3',
			'libxss1',
			'libxtst6',
			'xdg-utils',
			'libatspi2.0-0',
			'libuuid1',
			'libsecret-1-0',
			'libpulse0',
			'libpipewire-0.3-0',
			'libstdc++6',
			'libgcc-s1',
		],
	},
	rpm: {
		desktop: {
			entry: linuxDesktopEntryWithActions,
			desktopActions: linuxDesktopActions,
		},
		fpm: rpmBuildIdLinkFpmArgs,
		depends: [
			'gtk3',
			'libnotify',
			'nss',
			'libXScrnSaver',
			'libXtst',
			'xdg-utils',
			'at-spi2-core',
			'libuuid',
			'libsecret',
			'pulseaudio-libs',
			'pipewire-libs',
			'libstdc++',
			'libgcc',
		],
	},
	publish: null,
};
