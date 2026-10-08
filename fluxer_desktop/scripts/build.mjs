// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync} from 'node:child_process';
import * as fs from 'node:fs';
import {createRequire} from 'node:module';
import * as path from 'node:path';
import * as esbuild from 'esbuild';

const ROOT_DIR = path.resolve(import.meta.dirname, '..');
const SRC_DIR = path.join(ROOT_DIR, 'src');
const DIST_DIR = path.join(ROOT_DIR, 'dist');
const BUILD_INFO_FILE_NAME = 'build-info.json';
const BUILD_IN_PROGRESS_FILE_NAME = '.build-in-progress';
const BUILD_IN_PROGRESS_FILE = path.join(DIST_DIR, BUILD_IN_PROGRESS_FILE_NAME);
let compiledBuildChannel = '';
const NATIVE_DIR = path.join(ROOT_DIR, 'native');
const MONOREPO_ROOT = path.join(ROOT_DIR, '..');
const APP_DIST_DIR = path.join(MONOREPO_ROOT, 'fluxer_app', 'dist');
const RENDERER_DIST_ENTRY_NAME = 'renderer';
const RENDERER_DIST_DIR = path.join(DIST_DIR, RENDERER_DIST_ENTRY_NAME);
const SPLASH_SRC_DIR = path.join(SRC_DIR, 'splash');
const SPLASH_DIST_DIR = path.join(DIST_DIR, 'splash');
const SPLASH_PRELOAD_FILE_NAME = 'splash.cjs';
const REQUIRED_RENDERER_ENTRIES = Object.freeze(['index.html', 'assets']);
const FORBIDDEN_RENDERER_ENTRIES = Object.freeze(['sw.js', 'sw.js.map']);
const SUPPORTED_BUILD_ARGUMENTS = Object.freeze(['--shared-assets', '--use-shared-renderer']);
const MAIN_BOOTSTRAP_ENTRY_NAME = 'index';
const MAIN_APP_ENTRY_NAME = 'MainApp';
const MAIN_APP_OUTPUT_FILE_NAME = `${MAIN_APP_ENTRY_NAME}.js`;
const MAIN_APP_OUTPUT_FILE_DEFINE = '__FLUXER_MAIN_APP_OUTPUT_FILE__';
const MAIN_BOOTSTRAP_SOURCE = path.join(SRC_DIR, 'main', 'Bootstrap.ts');
const MAIN_APP_SOURCE = path.join(SRC_DIR, 'main', 'index.ts');
const DESKTOP_TSCONFIG = path.join(ROOT_DIR, 'tsconfig.json');
const requireModule = createRequire(import.meta.url);
const isProduction =
	process.env.NODE_ENV === 'production' ||
	process.env.FLUXER_DESKTOP_PRODUCTION === 'true' ||
	process.env.GITHUB_ACTIONS === 'true';
const skipNative = process.env.FLUXER_SKIP_NATIVE === 'true';
const embeddedBuildVersion = process.env.PUBLIC_BUILD_VERSION || process.env.BUILD_VERSION || '';
const embeddedReleaseChannel = process.env.PUBLIC_RELEASE_CHANNEL || process.env.RELEASE_CHANNEL || '';
const modulesEnabled = process.env.FLUXER_MODULES === '1' || process.env.FLUXER_MODULES === 'true';
const offlineBuild = process.env.FLUXER_OFFLINE === '1' || process.env.FLUXER_OFFLINE === 'true';
const publicBuildDefines = {
	'process.env.PUBLIC_BUILD_VERSION': JSON.stringify(embeddedBuildVersion),
	'process.env.BUILD_VERSION': JSON.stringify(embeddedBuildVersion),
	'process.env.PUBLIC_RELEASE_CHANNEL': JSON.stringify(embeddedReleaseChannel),
	'process.env.RELEASE_CHANNEL': JSON.stringify(embeddedReleaseChannel),
	'process.env.FLUXER_MODULES': JSON.stringify(modulesEnabled ? '1' : ''),
	'process.env.FLUXER_OFFLINE': JSON.stringify(offlineBuild ? '1' : ''),
};
const electronExternals = [
	'electron',
	'electron-log',
	'velopack',
	'@fluxer/app-store',
	'@fluxer/gateway-socket',
	'@fluxer/hardware-encoder',
	'@fluxer/webauthn',
	'hunspell-asm',
];
class UnsupportedDesktopBuildArgumentError extends Error {
	constructor(argument) {
		super(`Unsupported desktop build argument: ${argument}`);
		this.name = 'UnsupportedDesktopBuildArgumentError';
	}
}

class DuplicateDesktopBuildArgumentError extends Error {
	constructor(argument) {
		super(`Duplicate desktop build argument: ${argument}`);
		this.name = 'DuplicateDesktopBuildArgumentError';
	}
}

class SharedRendererConsumerConflictError extends Error {
	constructor() {
		super('--shared-assets produces the shared renderer and cannot be combined with --use-shared-renderer');
		this.name = 'SharedRendererConsumerConflictError';
	}
}

class RendererOutputIncompleteError extends Error {
	constructor(location, missing) {
		super(
			`Renderer output at ${location} is missing ${missing.join(', ')}. Run \`pnpm --filter fluxer_app build:desktop\`.`,
		);
		this.name = 'RendererOutputIncompleteError';
	}
}

class PackedRendererNotPrunedError extends Error {
	constructor(location) {
		super(`The renderer is a required module, so ${location} must be empty after the prune step.`);
		this.name = 'PackedRendererNotPrunedError';
	}
}

class RendererServiceWorkerPresentError extends Error {
	constructor(present) {
		super(
			`The desktop renderer bundle must not contain a service worker, found ${present.join(', ')} in ${RENDERER_DIST_DIR}.`,
		);
		this.name = 'RendererServiceWorkerPresentError';
	}
}

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
	for (const fileName of [
		'index.js',
		'index.d.ts',
		'binding.js',
		'binding.d.ts',
		'loader-diagnostics.cjs',
		'pure.cjs',
	]) {
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
	return fileName.endsWith('.node') || /\.so(?:\.|$)/.test(fileName) || /\.(?:dll|exe)$/i.test(fileName);
}

function electronArch() {
	return process.env.ELECTRON_ARCH || process.env.npm_config_arch || process.arch;
}

function primaryWinGameCaptureNodeFileName() {
	const arch = electronArch();
	const tag = platformTag(process.platform, arch);
	if (!tag || process.platform !== 'win32') {
		throw new Error(`Cannot resolve the primary win-game-capture node for ${process.platform}/${arch}`);
	}
	return `win-game-capture.${tag}.node`;
}

function removeStaleWinGameCaptureArtifacts(packageDir, primaryNodeFileName) {
	if (!fs.existsSync(packageDir)) return;
	const stale = fs
		.readdirSync(packageDir, {withFileTypes: true})
		.filter((entry) => {
			if (!entry.isFile()) return false;
			return (
				entry.name.startsWith('fluxer-game-hook.') ||
				entry.name.startsWith('fluxer-inject-helper.') ||
				entry.name.startsWith('fluxer-vulkan-layer.') ||
				(entry.name.startsWith('win-game-capture.') &&
					entry.name.endsWith('.node') &&
					entry.name !== primaryNodeFileName)
			);
		})
		.map((entry) => entry.name)
		.sort();
	for (const fileName of stale) {
		const artifactPath = path.join(packageDir, fileName);
		fs.rmSync(artifactPath);
		console.log(`  Removed stale @fluxer/win-game-capture artifact ${path.relative(ROOT_DIR, artifactPath)}`);
	}
}

function addWinGameCaptureRuntimeArtifacts(artifacts, tag) {
	artifacts.push({
		label: '@fluxer/win-game-capture',
		relativePath: `win-game-capture.${tag}.node`,
	});
}

function copyRuntimeArtifactsToInstalledPackages({label, packageDir}) {
	const primaryNodeFileName = label === '@fluxer/win-game-capture' ? primaryWinGameCaptureNodeFileName() : null;
	if (primaryNodeFileName) {
		removeStaleWinGameCaptureArtifacts(packageDir, primaryNodeFileName);
	}
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
		if (primaryNodeFileName) {
			removeStaleWinGameCaptureArtifacts(installedPackageDir, primaryNodeFileName);
		}
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

function expectedNativeRuntimeArtifacts(platform = process.platform, arch = electronArch()) {
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
		label: '@fluxer/hardware-encoder',
		relativePath: `hardware-encoder.${tag}.node`,
		runtimeFiles: ['index.js'],
	});
	artifacts.push({
		label: '@fluxer/app-store',
		relativePath: `app-store.${tag}.node`,
		runtimeFiles: ['index.js', 'loader-diagnostics.cjs', 'pure.cjs'],
	});
	artifacts.push({
		label: '@fluxer/gateway-socket',
		relativePath: `gateway-socket.${tag}.node`,
		runtimeFiles: ['index.js', 'loader-diagnostics.cjs', 'pure.cjs'],
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
		addWinGameCaptureRuntimeArtifacts(artifacts, tag);
		artifacts.push({
			label: '@fluxer/win-clipboard',
			relativePath: `win-clipboard.${tag}.node`,
		});
		artifacts.push({
			label: '@fluxer/win-shell',
			relativePath: `win-shell.${tag}.node`,
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
		label: '@fluxer/hardware-encoder',
		dirName: 'hardware-encoder',
		commands: [['pnpm', 'build']],
		jsEntry: 'index.js',
	});
	buildNativeAddon({
		label: '@fluxer/app-store',
		dirName: 'app-store',
		commands: [['pnpm', 'build']],
		jsEntry: 'index.js',
	});
	buildNativeAddon({
		label: '@fluxer/gateway-socket',
		dirName: 'gateway-socket',
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
	const result = await esbuild.build({
		entryPoints: {
			[MAIN_BOOTSTRAP_ENTRY_NAME]: MAIN_BOOTSTRAP_SOURCE,
			[MAIN_APP_ENTRY_NAME]: MAIN_APP_SOURCE,
		},
		bundle: true,
		splitting: true,
		platform: 'node',
		target: 'node20',
		format: 'esm',
		outdir: path.join(DIST_DIR, 'main'),
		entryNames: '[name]',
		chunkNames: 'chunks/[name]-[hash]',
		absWorkingDir: ROOT_DIR,
		minify: isProduction,
		sourcemap: true,
		external: electronExternals,
		tsconfig: DESKTOP_TSCONFIG,
		metafile: true,
		define: {
			'process.env.NODE_ENV': JSON.stringify(isProduction ? 'production' : 'development'),
			[MAIN_APP_OUTPUT_FILE_DEFINE]: JSON.stringify(MAIN_APP_OUTPUT_FILE_NAME),
			...publicBuildDefines,
		},
		banner: {
			js: `import { createRequire as __createRequire } from 'node:module'; const require = __createRequire(import.meta.url);`,
		},
	});
	assertMainProcessEntryOutput(
		result.metafile,
		MAIN_BOOTSTRAP_SOURCE,
		path.join(DIST_DIR, 'main', `${MAIN_BOOTSTRAP_ENTRY_NAME}.js`),
	);
	assertMainProcessEntryOutput(
		result.metafile,
		MAIN_APP_SOURCE,
		path.join(DIST_DIR, 'main', MAIN_APP_OUTPUT_FILE_NAME),
	);
	console.log('Main process build complete.');
}

class MainProcessBuildOutputError extends Error {
	constructor(entryPoint, expectedOutput, actualOutputs) {
		super(
			`Main process entry ${entryPoint} must emit ${expectedOutput} but emitted ${actualOutputs.length === 0 ? 'no entry output' : actualOutputs.join(', ')}`,
		);
		this.name = 'MainProcessBuildOutputError';
	}
}

function assertMainProcessEntryOutput(metafile, entryPoint, expectedOutput) {
	const actualOutputs = Object.entries(metafile.outputs)
		.filter(([, output]) => output.entryPoint != null && path.resolve(ROOT_DIR, output.entryPoint) === entryPoint)
		.map(([outputPath]) => path.resolve(ROOT_DIR, outputPath));
	if (actualOutputs.length !== 1 || actualOutputs[0] !== expectedOutput) {
		throw new MainProcessBuildOutputError(entryPoint, expectedOutput, actualOutputs);
	}
}

function copySplashAssets() {
	console.log('Copying splash assets...');
	fs.rmSync(SPLASH_DIST_DIR, {force: true, recursive: true});
	fs.cpSync(SPLASH_SRC_DIR, SPLASH_DIST_DIR, {recursive: true});
	fs.copyFileSync(
		path.join(SRC_DIR, 'preload', SPLASH_PRELOAD_FILE_NAME),
		path.join(DIST_DIR, 'preload', SPLASH_PRELOAD_FILE_NAME),
	);
	console.log(`  Splash assets copied to ${path.relative(ROOT_DIR, SPLASH_DIST_DIR)}`);
}

function parseBuildArguments(argumentList) {
	const seen = new Set();
	for (const argument of argumentList) {
		if (argument === '--') {
			continue;
		}
		if (!SUPPORTED_BUILD_ARGUMENTS.includes(argument)) {
			throw new UnsupportedDesktopBuildArgumentError(argument);
		}
		if (seen.has(argument)) {
			throw new DuplicateDesktopBuildArgumentError(argument);
		}
		seen.add(argument);
	}
	const options = Object.freeze({
		sharedAssetsOnly: seen.has('--shared-assets'),
		useSharedRenderer: seen.has('--use-shared-renderer'),
	});
	if (options.sharedAssetsOnly && options.useSharedRenderer) {
		throw new SharedRendererConsumerConflictError();
	}
	return options;
}

function cleanDistDirectory({useSharedRenderer}) {
	if (!fs.existsSync(DIST_DIR)) {
		return;
	}
	for (const entry of fs.readdirSync(DIST_DIR, {withFileTypes: true})) {
		if (useSharedRenderer && entry.name === RENDERER_DIST_ENTRY_NAME) {
			continue;
		}
		if (entry.name === BUILD_IN_PROGRESS_FILE_NAME) {
			continue;
		}
		fs.rmSync(path.join(DIST_DIR, entry.name), {force: true, recursive: true});
	}
}

function findMissingEntries(entries, rootDir) {
	return entries.filter((entry) => !fs.existsSync(path.join(rootDir, entry)));
}

function buildRendererBundle() {
	console.log('Building renderer bundle...');
	execFileSync('pnpm', ['--filter', 'fluxer_app', 'build:desktop'], {
		cwd: MONOREPO_ROOT,
		stdio: 'inherit',
		env: {...process.env, NODE_ENV: 'production'},
		shell: process.platform === 'win32',
	});
}

function copyRendererIntoDist() {
	const startedAt = Date.now();
	const missing = findMissingEntries(REQUIRED_RENDERER_ENTRIES, APP_DIST_DIR);
	if (missing.length > 0) {
		throw new RendererOutputIncompleteError(APP_DIST_DIR, missing);
	}
	fs.rmSync(RENDERER_DIST_DIR, {force: true, recursive: true});
	fs.cpSync(APP_DIST_DIR, RENDERER_DIST_DIR, {recursive: true});
	const present = FORBIDDEN_RENDERER_ENTRIES.filter((entry) => fs.existsSync(path.join(RENDERER_DIST_DIR, entry)));
	if (present.length > 0) {
		throw new RendererServiceWorkerPresentError(present);
	}
	console.log(
		`  Renderer assets copied to ${path.relative(ROOT_DIR, RENDERER_DIST_DIR)} in ${Date.now() - startedAt}ms`,
	);
}

function preloadBuildOptions(entryFileName, outputFileName) {
	return {
		entryPoints: [path.join(SRC_DIR, 'preload', entryFileName)],
		bundle: true,
		platform: 'node',
		target: 'node20',
		format: 'cjs',
		outfile: path.join(DIST_DIR, 'preload', outputFileName),
		minify: isProduction,
		sourcemap: true,
		external: electronExternals,
		tsconfig: DESKTOP_TSCONFIG,
		define: {
			'process.env.NODE_ENV': JSON.stringify(isProduction ? 'production' : 'development'),
			...publicBuildDefines,
		},
	};
}

async function buildPreload() {
	console.log('Building preload scripts...');
	await Promise.all([
		esbuild.build(preloadBuildOptions('index.ts', 'index.cjs')),
		esbuild.build(preloadBuildOptions('LegacyHarvestPreload.ts', 'legacy-harvest.cjs')),
	]);
	console.log('Preload script build complete.');
}

function runDesktopBuildStep(step) {
	const env = {...process.env};
	if (!env.WORKDIR && !env.GITHUB_WORKSPACE) {
		env.WORKDIR = MONOREPO_ROOT;
	}
	execFileSync(
		'cargo',
		[
			'run',
			'--manifest-path',
			path.join(MONOREPO_ROOT, 'tools', 'ci', 'Cargo.toml'),
			'--',
			'build-desktop',
			'--step',
			step,
		],
		{
			stdio: 'inherit',
			env,
		},
	);
}

function ensureBuildChannelFile() {
	runDesktopBuildStep('set_build_channel');
	compiledBuildChannel = readGeneratedBuildChannel();
}

function prunePackedRendererModules() {
	console.log('Pruning module-owned assets from the packed renderer...');
	const startedAt = Date.now();
	runDesktopBuildStep('strip_shell_renderer');
	const survivors = findMissingEntries(REQUIRED_RENDERER_ENTRIES, RENDERER_DIST_DIR);
	if (survivors.length !== REQUIRED_RENDERER_ENTRIES.length) {
		throw new PackedRendererNotPrunedError(RENDERER_DIST_DIR);
	}
	console.log(`  Packed renderer pruned in ${Date.now() - startedAt}ms`);
}

async function build() {
	const options = parseBuildArguments(process.argv.slice(2));
	ensureBuildChannelFile();
	if (options.sharedAssetsOnly) {
		console.log('Building shared renderer assets...');
		buildRendererBundle();
		cleanDistDirectory(options);
		copyRendererIntoDist();
		console.log('Shared renderer assets complete!');
		return;
	}
	console.log(`Building Electron app (${isProduction ? 'production' : 'development'})...`);
	buildNativeAddons();
	if (!options.useSharedRenderer) {
		buildRendererBundle();
	}
	fs.mkdirSync(DIST_DIR, {recursive: true});
	fs.writeFileSync(BUILD_IN_PROGRESS_FILE, '');
	cleanDistDirectory(options);
	fs.mkdirSync(path.join(DIST_DIR, 'main'), {recursive: true});
	fs.mkdirSync(path.join(DIST_DIR, 'preload'), {recursive: true});
	if (!options.useSharedRenderer) {
		copyRendererIntoDist();
	}
	if (modulesEnabled) {
		prunePackedRendererModules();
	}
	await Promise.all([buildMain(), buildPreload()]);
	copySplashAssets();
	writeBuildInfo();
	fs.rmSync(BUILD_IN_PROGRESS_FILE, {force: true});
	console.log('Build complete!');
}

function readGeneratedBuildChannel() {
	const source = fs.readFileSync(path.join(ROOT_DIR, 'src/common/BuildChannel.ts'), 'utf8');
	const match = /BUILD_CHANNEL = '([^']+)'/.exec(source);
	return match ? match[1] : '';
}

class BuildChannelDriftError extends Error {
	constructor(compiled, current) {
		super(
			`src/common/BuildChannel.ts changed from ${compiled} to ${current} while this build was running. It is gitignored and generated, so a concurrent build in the same worktree rewrote it. This build compiled ${compiled} and must not be recorded as ${current}.`,
		);
		this.name = 'BuildChannelDriftError';
	}
}

function writeBuildInfo() {
	const currentChannel = readGeneratedBuildChannel();
	if (currentChannel !== compiledBuildChannel) {
		throw new BuildChannelDriftError(compiledBuildChannel, currentChannel);
	}
	fs.writeFileSync(
		path.join(DIST_DIR, BUILD_INFO_FILE_NAME),
		`${JSON.stringify(
			{
				buildVersion: embeddedBuildVersion,
				releaseChannel: embeddedReleaseChannel,
				buildChannel: compiledBuildChannel,
				offlineBuild,
				modulesEnabled,
			},
			null,
			'\t',
		)}\n`,
	);
}

build().catch((error) => {
	console.error('Build failed:', error);
	process.exit(1);
});
