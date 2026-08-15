// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync} from 'node:child_process';
import * as fs from 'node:fs';
import {createRequire} from 'node:module';
import * as path from 'node:path';
import * as esbuild from 'esbuild';

const ROOT_DIR = path.resolve(import.meta.dirname, '..');
const SRC_DIR = path.join(ROOT_DIR, 'src');
const DIST_DIR = path.join(ROOT_DIR, 'dist');
const NATIVE_DIR = path.join(ROOT_DIR, 'native');
const requireModule = createRequire(import.meta.url);
const isProduction =
	process.env.NODE_ENV === 'production' ||
	process.env.FLUXER_DESKTOP_PRODUCTION === 'true' ||
	process.env.GITHUB_ACTIONS === 'true';
const skipNative = process.env.FLUXER_SKIP_NATIVE === 'true';
const embeddedBuildVersion = process.env.PUBLIC_BUILD_VERSION || process.env.BUILD_VERSION || '';
const embeddedReleaseChannel = process.env.PUBLIC_RELEASE_CHANNEL || process.env.RELEASE_CHANNEL || '';
const publicBuildDefines = {
	'process.env.PUBLIC_BUILD_VERSION': JSON.stringify(embeddedBuildVersion),
	'process.env.BUILD_VERSION': JSON.stringify(embeddedBuildVersion),
	'process.env.PUBLIC_RELEASE_CHANNEL': JSON.stringify(embeddedReleaseChannel),
	'process.env.RELEASE_CHANNEL': JSON.stringify(embeddedReleaseChannel),
};
const electronExternals = [
	'electron',
	'electron-log',
	'update-electron-app',
	'velopack',
	'@fluxer/webauthn',
	'@fluxer/webrtc-sender',
	'hunspell-asm',
];
const pathAliasPlugin = {
	name: 'path-alias',
	setup(build) {
		build.onResolve({filter: /^@electron\//}, (args) => {
			const relativePath = args.path.replace(/^@electron\//, '');
			const absolutePath = path.join(SRC_DIR, relativePath);
			const extensions = ['.tsx', '.ts', '.js', '.jsx'];
			for (const ext of extensions) {
				const fullPath = absolutePath + ext;
				if (fs.existsSync(fullPath)) {
					return {path: fullPath};
				}
			}
			for (const ext of extensions) {
				const indexPath = path.join(absolutePath, `index${ext}`);
				if (fs.existsSync(indexPath)) {
					return {path: indexPath};
				}
			}
			return {path: `${absolutePath}.tsx`};
		});
		build.onResolve({filter: /^@fluxer\/voice_engine_v2(?:\/.*)?$/}, (args) => {
			const packageSrcDir = path.join(ROOT_DIR, '..', 'packages', 'voice_engine_v2', 'src');
			if (args.path === '@fluxer/voice_engine_v2') {
				return {path: path.join(packageSrcDir, 'index.ts')};
			}
			const relativePath = args.path.replace(/^@fluxer\/voice_engine_v2\//, '');
			const directTsPath = path.join(packageSrcDir, `${relativePath}.ts`);
			if (fs.existsSync(directTsPath)) {
				return {path: directTsPath};
			}
			const indexTsPath = path.join(packageSrcDir, relativePath, 'index.ts');
			if (fs.existsSync(indexTsPath)) {
				return {path: indexTsPath};
			}
			return {path: path.join(packageSrcDir, relativePath)};
		});
	},
};

function findNodeBinary(rootDir) {
	const matches = [];
	for (const entry of fs.readdirSync(rootDir)) {
		if (entry.endsWith('.node')) matches.push(path.join(rootDir, entry));
	}
	return matches;
}

const ROOT_BIN_DIR = path.join(ROOT_DIR, 'node_modules', '.bin');

function toPackagePathParts(packageName) {
	const parts = packageName.split('/');
	if (parts.length === 2 && parts[0].startsWith('@')) {
		return parts;
	}
	return [packageName];
}

function addExistingPackageDir(packageDirs, packageDir) {
	if (!packageDir || !fs.existsSync(path.join(packageDir, 'package.json'))) {
		return;
	}
	const realPath = fs.realpathSync.native(packageDir);
	packageDirs.set(realPath, packageDir);
}

function findInstalledPackageDirs(packageName) {
	const packageDirs = new Map();
	const packagePathParts = toPackagePathParts(packageName);
	try {
		addExistingPackageDir(
			packageDirs,
			path.dirname(requireModule.resolve(`${packageName}/package.json`, {paths: [ROOT_DIR]})),
		);
	} catch {}
	addExistingPackageDir(packageDirs, path.join(ROOT_DIR, 'node_modules', ...packagePathParts));
	const pnpmRoot = path.join(ROOT_DIR, 'node_modules', '.pnpm');
	if (fs.existsSync(pnpmRoot)) {
		for (const entry of fs.readdirSync(pnpmRoot, {withFileTypes: true})) {
			if (!entry.isDirectory()) continue;
			addExistingPackageDir(packageDirs, path.join(pnpmRoot, entry.name, 'node_modules', ...packagePathParts));
		}
	}
	return Array.from(packageDirs.keys());
}

function addFilesFromDirectory(files, packageDir, relativeDir, predicate) {
	const absoluteDir = path.join(packageDir, relativeDir);
	if (!fs.existsSync(absoluteDir)) return;
	for (const entry of fs.readdirSync(absoluteDir, {withFileTypes: true})) {
		const relativePath = path.join(relativeDir, entry.name);
		const absolutePath = path.join(packageDir, relativePath);
		if (entry.isDirectory()) {
			addFilesFromDirectory(files, packageDir, relativePath, predicate);
		} else if (predicate(relativePath, absolutePath)) {
			files.add(relativePath);
		}
	}
}

function collectRuntimeArtifactPaths(packageDir) {
	const artifacts = new Set();
	for (const fileName of ['index.js', 'index.d.ts', 'binding.js', 'binding.d.ts', 'loader-diagnostics.cjs', 'pure.cjs']) {
		if (fs.existsSync(path.join(packageDir, fileName))) {
			artifacts.add(fileName);
		}
	}
	addFilesFromDirectory(artifacts, packageDir, 'lib', () => true);
	for (const entry of fs.readdirSync(packageDir, {withFileTypes: true})) {
		if (entry.isFile() && isNativeRuntimeSidecar(entry.name)) {
			artifacts.add(entry.name);
		}
	}
	return Array.from(artifacts).sort();
}

function isNativeRuntimeSidecar(fileName) {
	return (
		fileName.endsWith('.node') ||
		/\.so(?:\.|$)/.test(fileName) ||
		/\.(?:dll|exe)$/i.test(fileName) ||
		isWindowsNativeRuntimeManifest(fileName)
	);
}

function isWindowsNativeRuntimeManifest(fileName) {
	return (
		fileName === 'compatibility.json' || /^fluxer-vulkan-layer\.win32-(?:x64|ia32|arm64)-msvc\.json$/i.test(fileName)
	);
}

function addWinGameCaptureRuntimeArtifacts(artifacts, tag, arch) {
	const add = (relativePath) => {
		artifacts.push({
			label: '@fluxer/win-game-capture',
			relativePath,
			runtimeFiles: [],
		});
	};
	artifacts.push({
		label: '@fluxer/win-game-capture',
		relativePath: `win-game-capture.${tag}.node`,
	});
	add(`fluxer-game-hook.${tag}.dll`);
	add(`fluxer-inject-helper.${tag}.exe`);
	add(`fluxer-vulkan-layer.${tag}.dll`);
	add(`fluxer-vulkan-layer.${tag}.json`);
	if (arch === 'x64') {
		add('fluxer-game-hook.win32-ia32-msvc.dll');
		add('fluxer-inject-helper.win32-ia32-msvc.exe');
	}
}

function copyRuntimeArtifactsToInstalledPackages({label, packageDir}) {
	const artifacts = collectRuntimeArtifactPaths(packageDir);
	if (artifacts.length === 0) {
		return;
	}
	const sourceRealPath = fs.realpathSync.native(packageDir);
	const installedPackageDirs = findInstalledPackageDirs(label).filter(
		(installedPackageDir) => installedPackageDir !== sourceRealPath,
	);
	if (installedPackageDirs.length === 0) {
		return;
	}
	for (const installedPackageDir of installedPackageDirs) {
		for (const artifact of artifacts) {
			const sourcePath = path.join(packageDir, artifact);
			const targetPath = path.join(installedPackageDir, artifact);
			fs.mkdirSync(path.dirname(targetPath), {recursive: true});
			fs.copyFileSync(sourcePath, targetPath);
		}
		console.log(
			`  Synced ${artifacts.length} runtime artifact(s) for ${label} into ${path.relative(ROOT_DIR, installedPackageDir)}`,
		);
	}
}

function platformTag(platform, arch) {
	if (platform === 'darwin') return `darwin-${arch}`;
	if (platform === 'win32') return `win32-${arch}-msvc`;
	if (platform === 'linux') return `linux-${arch}-gnu`;
	return null;
}

function expectedNativeRuntimeArtifacts(platform = process.platform, arch = process.env.ELECTRON_ARCH || process.arch) {
	if (platform === 'darwin' && arch === 'universal') {
		return [
			...expectedNativeRuntimeArtifactsForArch(platform, 'arm64'),
			...expectedNativeRuntimeArtifactsForArch(platform, 'x64'),
		];
	}
	return expectedNativeRuntimeArtifactsForArch(platform, arch);
}

function expectedNativeRuntimeArtifactsForArch(platform, arch) {
	const tag = platformTag(platform, arch);
	if (!tag) return [];
	const artifacts = [];
	artifacts.push({
		label: '@fluxer/webauthn',
		relativePath: `webauthn.${tag}.node`,
		runtimeFiles: ['index.js', 'loader-diagnostics.cjs', 'pure.cjs'],
	});
	artifacts.push({
		label: '@fluxer/webrtc-sender',
		relativePath: `webrtc-sender.${tag}.node`,
		runtimeFiles: ['index.js'],
	});
	if (platform === 'darwin') {
		artifacts.push({
			label: '@fluxer/mac-app-audio',
			relativePath: `mac-app-audio.darwin-${arch}.node`,
		});
		artifacts.push({
			label: '@fluxer/mac-screen-capture',
			relativePath: `mac-screen-capture.darwin-${arch}.node`,
		});
		artifacts.push({
			label: '@fluxer/mac-clipboard',
			relativePath: `mac-clipboard.darwin-${arch}.node`,
		});
		artifacts.push({
			label: '@fluxer/mac-sysctl',
			relativePath: `mac-sysctl.darwin-${arch}.node`,
		});
		artifacts.push({
			label: '@fluxer/mac-tcc',
			relativePath: `mac-tcc.darwin-${arch}.node`,
		});
		artifacts.push({
			label: '@fluxer/macos-input-hook',
			relativePath: `macos-input-hook.darwin-${arch}.node`,
		});
		artifacts.push({
			label: '@fluxer/platform-info',
			relativePath: `platform-info.${tag}.node`,
			runtimeFiles: ['index.js', 'loader-diagnostics.cjs', 'pure.cjs'],
		});
	} else if (platform === 'win32') {
		artifacts.push({
			label: '@fluxer/win-process-loopback',
			relativePath: `win-process-loopback.${tag}.node`,
		});
		addWinGameCaptureRuntimeArtifacts(artifacts, tag, arch);
		artifacts.push({
			label: '@fluxer/win-clipboard',
			relativePath: `win-clipboard.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/win-shell',
			relativePath: `win-shell.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/win-toast',
			relativePath: `win-toast.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/windows-input-hook',
			relativePath: `windows-input-hook.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/platform-info',
			relativePath: `platform-info.${tag}.node`,
			runtimeFiles: ['index.js', 'loader-diagnostics.cjs', 'pure.cjs'],
		});
	} else if (platform === 'linux') {
		artifacts.push({
			label: '@fluxer/linux-audio-capture',
			relativePath: `linux-audio-capture.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/linux-screen-capture',
			relativePath: `linux-screen-capture.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/linux-portals',
			relativePath: `linux-portals.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/linux-notifications',
			relativePath: `linux-notifications.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/linux-evdev',
			relativePath: `linux-evdev.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/system-hunspell',
			relativePath: `system-hunspell.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/linux-input-hook',
			relativePath: `linux-input-hook.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/platform-info',
			relativePath: `platform-info.${tag}.node`,
			runtimeFiles: ['index.js', 'loader-diagnostics.cjs', 'pure.cjs'],
		});
	}
	return artifacts;
}

function verifyInstalledNativeArtifacts() {
	if (skipNative) return;
	const missing = [];
	for (const artifact of expectedNativeRuntimeArtifacts()) {
		const packageDirs = findInstalledPackageDirs(artifact.label);
		if (packageDirs.length === 0) {
			missing.push(`${artifact.label}: package is not installed`);
			continue;
		}
		for (const packageDir of packageDirs) {
			for (const runtimeFile of artifact.runtimeFiles ?? ['index.js', 'loader-diagnostics.cjs']) {
				const runtimePath = path.join(packageDir, runtimeFile);
				if (!fs.existsSync(runtimePath)) {
					missing.push(`${artifact.label}: missing ${runtimeFile} in ${path.relative(ROOT_DIR, packageDir)}`);
				}
			}
			const artifactPath = path.join(packageDir, artifact.relativePath);
			if (!fs.existsSync(artifactPath)) {
				missing.push(`${artifact.label}: missing ${artifact.relativePath} in ${path.relative(ROOT_DIR, packageDir)}`);
			}
		}
	}
	if (missing.length > 0) {
		throw new Error(`Native runtime artifact sync failed:\n${missing.map((entry) => `  - ${entry}`).join('\n')}`);
	}
}

function runNativeCommand(packageDir, command) {
	const [bin, ...args] = command;
	console.log(`  $ ${command.join(' ')}`);
	const env = {
		...process.env,
		PATH: `${ROOT_BIN_DIR}${path.delimiter}${process.env.PATH || ''}`,
	};
	if (isProduction) {
		env.NODE_ENV = 'production';
		env.FLUXER_DESKTOP_PRODUCTION = 'true';
	}
	execFileSync(bin, args, {
		cwd: packageDir,
		stdio: 'inherit',
		env,
		shell: process.platform === 'win32',
	});
}

function buildNativeAddon({label, dirName, commands, jsEntry = 'lib/index.js'}) {
	const packageDir = path.join(NATIVE_DIR, dirName);
	if (!fs.existsSync(packageDir)) {
		throw new Error(`Native addon directory missing: ${packageDir}`);
	}
	const startedAt = Date.now();
	console.log(`Building native addon ${label}...`);
	for (const command of commands) {
		runNativeCommand(packageDir, command);
	}
	const jsEntryPath = path.join(packageDir, jsEntry);
	if (!fs.existsSync(jsEntryPath)) {
		throw new Error(`${label}: JS entry missing at ${jsEntryPath} after build`);
	}
	const nodeBinaries = findNodeBinary(packageDir);
	if (nodeBinaries.length === 0) {
		throw new Error(`${label}: no Rust .node binary produced in the package root`);
	}
	console.log(
		`  ${label} built in ${Date.now() - startedAt}ms (binaries: ${nodeBinaries.map((file) => path.relative(packageDir, file)).join(', ')})`,
	);
	copyRuntimeArtifactsToInstalledPackages({label, packageDir});
}

function buildNativeAddons() {
	if (skipNative) {
		console.log('Skipping native addons (FLUXER_SKIP_NATIVE=true).');
		return;
	}
	buildNativeAddon({
		label: '@fluxer/webauthn',
		dirName: 'webauthn',
		commands: [['pnpm', 'build']],
		jsEntry: 'index.js',
	});
	buildNativeAddon({
		label: '@fluxer/webrtc-sender',
		dirName: 'webrtc-sender',
		commands: [['pnpm', 'build']],
		jsEntry: 'index.js',
	});
	if (process.platform === 'darwin') {
		buildNativeAddon({
			label: '@fluxer/mac-app-audio',
			dirName: 'mac-app-audio',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/mac-screen-capture',
			dirName: 'mac-screen-capture',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/mac-clipboard',
			dirName: 'mac-clipboard',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/mac-sysctl',
			dirName: 'mac-sysctl',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/mac-tcc',
			dirName: 'mac-tcc',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/macos-input-hook',
			dirName: 'macos-input-hook',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/platform-info',
			dirName: 'platform-info',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		verifyInstalledNativeArtifacts();
		return;
	}
	if (process.platform === 'win32') {
		buildNativeAddon({
			label: '@fluxer/win-process-loopback',
			dirName: 'win-process-loopback',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/win-clipboard',
			dirName: 'win-clipboard',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/win-shell',
			dirName: 'win-shell',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/win-toast',
			dirName: 'win-toast',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/windows-input-hook',
			dirName: 'windows-input-hook',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/win-game-capture',
			dirName: 'win-game-capture',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/platform-info',
			dirName: 'platform-info',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		verifyInstalledNativeArtifacts();
		return;
	}
	if (process.platform === 'linux') {
		buildNativeAddon({
			label: '@fluxer/linux-audio-capture',
			dirName: 'linux-audio-capture',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/linux-screen-capture',
			dirName: 'linux-screen-capture',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/linux-portals',
			dirName: 'linux-portals',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/linux-notifications',
			dirName: 'linux-notifications',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/linux-evdev',
			dirName: 'linux-evdev',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/system-hunspell',
			dirName: 'system-hunspell',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/linux-input-hook',
			dirName: 'linux-input-hook',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		buildNativeAddon({
			label: '@fluxer/platform-info',
			dirName: 'platform-info',
			commands: [['pnpm', 'build']],
			jsEntry: 'index.js',
		});
		verifyInstalledNativeArtifacts();
		return;
	}
	console.log(`No native audio addon for platform ${process.platform}; skipping.`);
}

async function buildMain() {
	console.log('Building main process...');
	await Promise.all([
		esbuild.build({
			entryPoints: [path.join(SRC_DIR, 'main', 'Bootstrap.ts')],
			bundle: true,
			platform: 'node',
			target: 'node20',
			format: 'esm',
			outfile: path.join(DIST_DIR, 'main', 'index.js'),
			minify: isProduction,
			sourcemap: true,
			external: electronExternals,
			plugins: [pathAliasPlugin],
			define: {
				'process.env.NODE_ENV': JSON.stringify(isProduction ? 'production' : 'development'),
				...publicBuildDefines,
			},
			banner: {
				js: `import { createRequire as __createRequire } from 'node:module'; const require = __createRequire(import.meta.url);`,
			},
		}),
		esbuild.build({
			entryPoints: [path.join(SRC_DIR, 'main', 'index.ts')],
			bundle: true,
			platform: 'node',
			target: 'node20',
			format: 'esm',
			outfile: path.join(DIST_DIR, 'main', 'MainApp.js'),
			minify: isProduction,
			sourcemap: true,
			external: electronExternals,
			plugins: [pathAliasPlugin],
			define: {
				'process.env.NODE_ENV': JSON.stringify(isProduction ? 'production' : 'development'),
				...publicBuildDefines,
			},
			banner: {
				js: `import { createRequire as __createRequire } from 'node:module'; const require = __createRequire(import.meta.url);`,
			},
		}),
	]);
	console.log('Main process build complete.');
}

async function buildPreload() {
	console.log('Building preload script...');
	await esbuild.build({
		entryPoints: [path.join(SRC_DIR, 'preload', 'index.ts')],
		bundle: true,
		platform: 'node',
		target: 'node20',
		format: 'cjs',
		outfile: path.join(DIST_DIR, 'preload', 'index.cjs'),
		minify: isProduction,
		sourcemap: true,
		external: electronExternals,
		plugins: [pathAliasPlugin],
		define: {
			'process.env.NODE_ENV': JSON.stringify(isProduction ? 'production' : 'development'),
			...publicBuildDefines,
		},
	});
	console.log('Preload script build complete.');
}

function ensureBuildChannelFile() {
	execFileSync(
		'cargo',
		[
			'run',
			'--manifest-path',
			path.join(ROOT_DIR, '..', 'tools', 'ci', 'Cargo.toml'),
			'--',
			'build-desktop',
			'--step',
			'set_build_channel',
		],
		{
			stdio: 'inherit',
			env: process.env,
		},
	);
}

async function build() {
	console.log(`Building Electron app (${isProduction ? 'production' : 'development'})...`);
	ensureBuildChannelFile();
	if (fs.existsSync(DIST_DIR)) {
		fs.rmSync(DIST_DIR, {recursive: true});
	}
	fs.mkdirSync(path.join(DIST_DIR, 'main'), {recursive: true});
	fs.mkdirSync(path.join(DIST_DIR, 'preload'), {recursive: true});
	buildNativeAddons();
	await Promise.all([buildMain(), buildPreload()]);
	console.log('Build complete!');
}

build().catch((error) => {
	console.error('Build failed:', error);
	process.exit(1);
});
