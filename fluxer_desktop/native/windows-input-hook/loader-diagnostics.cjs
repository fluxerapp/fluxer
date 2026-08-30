// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync, readdirSync, readFileSync, statSync} = require('node:fs');
const os = require('node:os');
const {basename} = require('node:path');
const {spawnSync} = require('node:child_process');
const NATIVE_LOAD_ERROR_MARKER = Symbol.for('fluxer.nativeLoadError');
const MAX_TEXT_LENGTH = 6000;
const MAX_DIRECTORY_ENTRIES = 80;
const PREFLIGHT_CHILD_ENV = 'FLUXER_NATIVE_MODULE_PREFLIGHT_CHILD';
const PROBE_TIMEOUT_ENV = 'FLUXER_NATIVE_PROBE_TIMEOUT_MS';
const DEFAULT_PROBE_TIMEOUT_MS = 30000;
const PROBE_REACHED_REQUIRE_MARKER = '__fluxer_native_probe_reached_require__';
const PROBE_SOURCE = `require('node:fs').writeSync(1, ${JSON.stringify(PROBE_REACHED_REQUIRE_MARKER)});require(process.argv[1]);`;

function trimText(value, limit = MAX_TEXT_LENGTH) {
	const text = Buffer.isBuffer(value) ? value.toString('utf8') : String(value ?? '');
	const trimmed = text.trim();
	if (!trimmed) return null;
	return trimmed.length > limit ? `${trimmed.slice(0, limit)}\n...<truncated>` : trimmed;
}

function errorDiagnostic(error) {
	if (!error) return null;
	if (error instanceof Error) {
		return {
			name: error.name || 'Error',
			message: error.message,
			code: error.code || null,
			stack: trimText(error.stack || error.message),
		};
	}
	return {
		name: typeof error,
		message: trimText(String(error)),
		code: null,
		stack: null,
	};
}

function formatErrorDiagnostic(diagnostic) {
	if (!diagnostic) return null;
	const lines = [];
	if (diagnostic.code) lines.push(`code=${diagnostic.code}`);
	if (diagnostic.stack) lines.push(diagnostic.stack);
	else if (diagnostic.message) lines.push(diagnostic.message);
	return trimText(lines.join('\n'));
}

function fileDiagnostic(filePath) {
	if (!filePath) return {path: null, exists: false, error: 'not resolved'};
	try {
		const stat = statSync(filePath);
		return {
			path: filePath,
			exists: true,
			size: stat.size,
			mode: `0${(stat.mode & 0o777).toString(8)}`,
			mtime: stat.mtime.toISOString(),
			isFile: stat.isFile(),
			isDirectory: stat.isDirectory(),
		};
	} catch (error) {
		const reason = error instanceof Error ? error.message : String(error);
		return {path: filePath, exists: false, error: reason};
	}
}

function formatFileDiagnostic(diagnostic) {
	if (!diagnostic) return 'not resolved';
	if (!diagnostic.exists) return `exists=false, statError=${diagnostic.error || '<unknown>'}`;
	return [
		`exists=true`,
		`size=${diagnostic.size}`,
		`mode=${diagnostic.mode}`,
		`mtime=${diagnostic.mtime}`,
		`isFile=${diagnostic.isFile}`,
	].join(', ');
}

function directoryDiagnostic(dirPath) {
	if (!dirPath) return {path: null, ok: false, error: 'not resolved', entries: [], total: 0, omitted: 0};
	try {
		const entries = readdirSync(dirPath, {withFileTypes: true}).map((entry) => ({
			name: entry.name,
			type: entry.isDirectory() ? 'directory' : entry.isFile() ? 'file' : 'other',
		}));
		entries.sort((a, b) => a.name.localeCompare(b.name));
		const visible = entries.slice(0, MAX_DIRECTORY_ENTRIES);
		return {
			path: dirPath,
			ok: true,
			entries: visible,
			total: entries.length,
			omitted: Math.max(0, entries.length - visible.length),
		};
	} catch (error) {
		return {
			path: dirPath,
			ok: false,
			error: error instanceof Error ? error.message : String(error),
			entries: [],
			total: 0,
			omitted: 0,
		};
	}
}

function formatDirectoryDiagnostic(diagnostic) {
	if (!diagnostic) return '<unavailable>';
	if (!diagnostic.ok) return `directory listing failed: ${diagnostic.error || '<unknown>'}`;
	const entries = diagnostic.entries.map((entry) => `${entry.name}${entry.type === 'directory' ? '/' : ''}`);
	const suffix = diagnostic.omitted > 0 ? [`...<${diagnostic.omitted} more entries>`] : [];
	return [...entries, ...suffix].join('\n') || '<empty>';
}

function selectedEnvironmentNames(skipNativeProbeEnv) {
	const names = [
		'ELECTRON_RUN_AS_NODE',
		'FLUXER_NATIVE_MODULE_PREFLIGHT_CHILD',
		'LD_LIBRARY_PATH',
		'DYLD_LIBRARY_PATH',
		'DISPLAY',
		'WAYLAND_DISPLAY',
		'XDG_CURRENT_DESKTOP',
		'XDG_SESSION_TYPE',
		'DBUS_SESSION_BUS_ADDRESS',
		'PULSE_SERVER',
		'PIPEWIRE_REMOTE',
		'PATH',
	];
	if (skipNativeProbeEnv) names.push(skipNativeProbeEnv);
	return names;
}

function environmentDiagnostics(skipNativeProbeEnv) {
	return Object.fromEntries(
		selectedEnvironmentNames(skipNativeProbeEnv).map((name) => [name, process.env[name] ?? null]),
	);
}

function formatEnvironment(diagnostic) {
	return Object.entries(diagnostic)
		.map(([name, value]) => `${name}=${value ?? '<unset>'}`)
		.join('\n');
}

function runtimeDiagnostics() {
	const versions = process.versions || {};
	let reportHeader = null;
	if (process.report && typeof process.report.getReport === 'function') {
		try {
			reportHeader = process.report.getReport().header || null;
		} catch {
			reportHeader = null;
		}
	}
	const glibcRuntime = versions.glibcVersionRuntime || reportHeader?.glibcVersionRuntime || '<unknown>';
	const glibcCompiler = versions.glibcVersionCompiler || reportHeader?.glibcVersionCompiler || '<unknown>';
	return {
		node: versions.node || null,
		electron: versions.electron || null,
		modules: versions.modules || null,
		napi: versions.napi || null,
		v8: versions.v8 || null,
		uv: versions.uv || null,
		openssl: versions.openssl || null,
		glibcRuntime,
		glibcCompiler,
		platform: process.platform,
		arch: process.arch,
		osType: os.type(),
		osRelease: os.release(),
		osVersion: typeof os.version === 'function' ? os.version() : null,
		execPath: process.execPath,
		resourcesPath: process.resourcesPath || null,
		cwd: process.cwd(),
	};
}

function formatRuntimeDiagnostics(diagnostic) {
	return [
		`node=${diagnostic.node || '<unknown>'}`,
		`electron=${diagnostic.electron || '<none>'}`,
		`modules=${diagnostic.modules || '<unknown>'}`,
		`napi=${diagnostic.napi || '<unknown>'}`,
		`v8=${diagnostic.v8 || '<unknown>'}`,
		`uv=${diagnostic.uv || '<unknown>'}`,
		`openssl=${diagnostic.openssl || '<unknown>'}`,
		`glibcRuntime=${diagnostic.glibcRuntime || '<unknown>'}`,
		`glibcCompiler=${diagnostic.glibcCompiler || '<unknown>'}`,
		`process=${diagnostic.platform}/${diagnostic.arch}`,
		`os=${diagnostic.osType} ${diagnostic.osRelease} ${diagnostic.osVersion || '<unknown>'}`,
		`execPath=${diagnostic.execPath}`,
		`resourcesPath=${diagnostic.resourcesPath || '<unknown>'}`,
		`cwd=${diagnostic.cwd}`,
	].join('\n');
}

const REDISTRIBUTABLE_RUNTIME_PATTERNS = [
	/^vcruntime\d+(?:_\d+)?\.dll$/i,
	/^msvcp\d+(?:_\d+)?\.dll$/i,
	/^msvcr\d+(?:_\d+)?\.dll$/i,
	/^concrt\d+\.dll$/i,
	/^vcamp\d+\.dll$/i,
	/^vcomp\d+\.dll$/i,
];

function readPeImports(filePath) {
	let buffer;
	try {
		buffer = readFileSync(filePath);
	} catch {
		return null;
	}
	if (buffer.length < 0x40) return null;
	const peOffset = buffer.readUInt32LE(0x3c);
	if (peOffset <= 0 || peOffset + 24 >= buffer.length) return null;
	if (buffer.readUInt32LE(peOffset) !== 0x4550) return null;
	const coffOffset = peOffset + 4;
	const numberOfSections = buffer.readUInt16LE(coffOffset + 2);
	const sizeOfOptionalHeader = buffer.readUInt16LE(coffOffset + 16);
	const optionalHeaderOffset = coffOffset + 20;
	if (optionalHeaderOffset + sizeOfOptionalHeader > buffer.length) return null;
	const magic = buffer.readUInt16LE(optionalHeaderOffset);
	if (magic !== 0x10b && magic !== 0x20b) return null;
	const dataDirectoriesOffset = optionalHeaderOffset + (magic === 0x20b ? 112 : 96);
	const importEntryOffset = dataDirectoriesOffset + 8;
	if (importEntryOffset + 8 > buffer.length) return null;
	const importRva = buffer.readUInt32LE(importEntryOffset);
	if (importRva === 0) return [];
	const sections = [];
	const sectionTableOffset = optionalHeaderOffset + sizeOfOptionalHeader;
	for (let i = 0; i < numberOfSections; i++) {
		const base = sectionTableOffset + i * 40;
		if (base + 40 > buffer.length) return null;
		sections.push({
			virtualSize: buffer.readUInt32LE(base + 8),
			virtualAddress: buffer.readUInt32LE(base + 12),
			rawSize: buffer.readUInt32LE(base + 16),
			rawPointer: buffer.readUInt32LE(base + 20),
		});
	}
	const rvaToOffset = (rva) => {
		for (const s of sections) {
			const span = Math.max(s.virtualSize, s.rawSize);
			if (rva >= s.virtualAddress && rva < s.virtualAddress + span) {
				return rva - s.virtualAddress + s.rawPointer;
			}
		}
		return -1;
	};
	const readCString = (offset) => {
		let end = offset;
		while (end < buffer.length && buffer[end] !== 0) end++;
		return buffer.toString('ascii', offset, end);
	};
	const importTableOffset = rvaToOffset(importRva);
	if (importTableOffset < 0) return [];
	const imports = new Set();
	for (let i = 0; i < 1024; i++) {
		const base = importTableOffset + i * 20;
		if (base + 20 > buffer.length) break;
		const lookupRva = buffer.readUInt32LE(base);
		const nameRva = buffer.readUInt32LE(base + 12);
		const iatRva = buffer.readUInt32LE(base + 16);
		if (lookupRva === 0 && nameRva === 0 && iatRva === 0) break;
		const nameOffset = rvaToOffset(nameRva);
		if (nameOffset < 0) continue;
		const name = readCString(nameOffset);
		if (name) imports.add(name);
	}
	return Array.from(imports);
}

function windowsImportProbe(nativePath) {
	const imports = readPeImports(nativePath);
	if (imports === null) return null;
	const sortedImports = [...imports].sort((a, b) => a.toLowerCase().localeCompare(b.toLowerCase()));
	const redistributable = sortedImports.filter((dll) =>
		REDISTRIBUTABLE_RUNTIME_PATTERNS.some((pattern) => pattern.test(dll)),
	);
	return {
		command: ['pe-imports', nativePath],
		status: 0,
		signal: null,
		error: null,
		stdout: sortedImports.join('\n') || null,
		stderr: null,
		missing: [],
		redistributable,
	};
}

function dependencyProbe(nativePath) {
	if (!nativePath || !existsSync(nativePath)) return null;
	if (process.platform === 'win32') return windowsImportProbe(nativePath);
	const command =
		process.platform === 'linux'
			? ['ldd', nativePath]
			: process.platform === 'darwin'
				? ['otool', '-L', nativePath]
				: null;
	if (!command) return null;
	const [bin, ...args] = command;
	const result = spawnSync(bin, args, {
		encoding: 'utf8',
		timeout: 4000,
		stdio: ['ignore', 'pipe', 'pipe'],
	});
	const stdout = trimText(result.stdout);
	const stderr = trimText(result.stderr);
	const missing =
		process.platform === 'linux' && stdout
			? stdout
					.split('\n')
					.map((line) => line.trim())
					.filter((line) => line.includes('not found'))
			: [];
	return {
		command,
		status: result.status,
		signal: result.signal || null,
		error: result.error ? result.error.message : null,
		stdout,
		stderr,
		missing,
		redistributable: [],
	};
}

function formatDependencyProbe(diagnostic) {
	if (!diagnostic) return null;
	const status = diagnostic.error
		? `error=${diagnostic.error}`
		: diagnostic.signal
			? `signal=${diagnostic.signal}`
			: `status=${diagnostic.status}`;
	return [
		`$ ${diagnostic.command.join(' ')}`,
		status,
		diagnostic.missing?.length ? `missing:\n${diagnostic.missing.join('\n')}` : null,
		diagnostic.redistributable?.length
			? `redistributableRuntimeImports (require VC++ redist on host):\n${diagnostic.redistributable.join('\n')}`
			: null,
		diagnostic.stdout ? `stdout:\n${diagnostic.stdout}` : null,
		diagnostic.stderr ? `stderr:\n${diagnostic.stderr}` : null,
	]
		.filter(Boolean)
		.join('\n');
}

function formatExtraDiagnostic(diagnostic) {
	if (!diagnostic) return null;
	if (typeof diagnostic === 'string') return diagnostic;
	if (typeof diagnostic === 'object' && diagnostic.name && diagnostic.text) {
		return `${diagnostic.name}:\n${diagnostic.text}`;
	}
	return `extra:\n${trimText(JSON.stringify(diagnostic, null, 2))}`;
}

function collectNativeDiagnostics({
	moduleName,
	nativePath,
	nativeRoot,
	packageDir,
	reason,
	cause,
	skipNativeProbeEnv,
	extraDiagnostics = [],
}) {
	return {
		schemaVersion: 1,
		moduleName,
		reason,
		target: {
			platform: process.platform,
			arch: process.arch,
		},
		packageDir: packageDir || null,
		nativeRoot: nativeRoot || null,
		nativePath: nativePath || null,
		nativeFile: nativePath ? basename(nativePath) : null,
		nativeFileStat: fileDiagnostic(nativePath),
		runtime: runtimeDiagnostics(),
		environment: environmentDiagnostics(skipNativeProbeEnv),
		nativeRootEntries: directoryDiagnostic(nativeRoot),
		dependencyProbe: dependencyProbe(nativePath),
		extraDiagnostics: extraDiagnostics.filter(Boolean),
		cause: errorDiagnostic(cause),
	};
}

function formatNativeDiagnostics(diagnostics) {
	const sections = [
		`module=${diagnostics.moduleName}`,
		`reason=${diagnostics.reason}`,
		`target=${diagnostics.target.platform}/${diagnostics.target.arch}`,
		`packageDir=${diagnostics.packageDir || '<unknown>'}`,
		`nativeRoot=${diagnostics.nativeRoot || '<unknown>'}`,
		`nativePath=${diagnostics.nativePath || '<unknown>'}`,
		`nativeFile=${diagnostics.nativeFile || '<unknown>'}`,
		`nativeFileStat=${formatFileDiagnostic(diagnostics.nativeFileStat)}`,
		`runtime:\n${formatRuntimeDiagnostics(diagnostics.runtime)}`,
		`environment:\n${formatEnvironment(diagnostics.environment)}`,
		`nativeRootEntries:\n${formatDirectoryDiagnostic(diagnostics.nativeRootEntries)}`,
		...diagnostics.extraDiagnostics.map(formatExtraDiagnostic).filter(Boolean),
	];
	const dependencyOutput = formatDependencyProbe(diagnostics.dependencyProbe);
	if (dependencyOutput) sections.push(`dependencyProbe:\n${dependencyOutput}`);
	const causeText = formatErrorDiagnostic(diagnostics.cause);
	if (causeText) sections.push(`cause:\n${causeText}`);
	return sections.join('\n');
}

function isNativeLoadError(error) {
	return Boolean(error?.[NATIVE_LOAD_ERROR_MARKER]);
}

function createNativeLoadError({
	moduleName,
	nativePath,
	nativeRoot,
	packageDir,
	reason,
	cause,
	skipNativeProbeEnv,
	extraDiagnostics = [],
}) {
	if (isNativeLoadError(cause)) return cause;
	const diagnostics = collectNativeDiagnostics({
		moduleName,
		nativePath,
		nativeRoot,
		packageDir,
		reason,
		cause,
		skipNativeProbeEnv,
		extraDiagnostics,
	});
	const error = new Error(`${moduleName} native module failed to load.\n${formatNativeDiagnostics(diagnostics)}`);
	error.name = 'NativeModuleLoadError';
	error[NATIVE_LOAD_ERROR_MARKER] = true;
	error.nativeDiagnostics = diagnostics;
	error.toJSON = () => ({
		name: error.name,
		message: error.message,
		nativeDiagnostics: diagnostics,
	});
	if (cause) error.cause = cause;
	return error;
}

function resolveProbeTimeoutMs(explicitTimeoutMs) {
	const configured = Number.parseInt(process.env[PROBE_TIMEOUT_ENV] ?? '', 10);
	if (Number.isFinite(configured) && configured > 0) return configured;
	return explicitTimeoutMs ?? DEFAULT_PROBE_TIMEOUT_MS;
}

function probeReachedRequire(result) {
	return typeof result.stdout === 'string' && result.stdout.includes(PROBE_REACHED_REQUIRE_MARKER);
}

function stripProbeMarker(value) {
	return typeof value === 'string' ? value.split(PROBE_REACHED_REQUIRE_MARKER).join('') : value;
}

function probeNativeBinary({moduleName, nativePath, nativeRoot, packageDir, skipNativeProbeEnv, timeoutMs}) {
	if (!skipNativeProbeEnv || process.env[skipNativeProbeEnv] === '1') {
		return null;
	}
	if (process.env[PREFLIGHT_CHILD_ENV] === '1') {
		return null;
	}
	const result = spawnSync(process.execPath, ['-e', PROBE_SOURCE, nativePath], {
		env: {...process.env, ELECTRON_RUN_AS_NODE: '1', [skipNativeProbeEnv]: '1'},
		encoding: 'utf8',
		stdio: ['ignore', 'pipe', 'pipe'],
		timeout: resolveProbeTimeoutMs(timeoutMs),
	});
	if (result.status === 0) return null;
	if (!probeReachedRequire(result)) return null;
	const reason = result.error
		? result.error.message
		: result.signal
			? `safety probe terminated by signal ${result.signal}`
			: `safety probe exited with code ${result.status}`;
	const probeStdout = trimText(stripProbeMarker(result.stdout));
	return createNativeLoadError({
		moduleName,
		nativePath,
		nativeRoot,
		packageDir,
		reason,
		skipNativeProbeEnv,
		extraDiagnostics: [
			probeStdout ? {name: 'probeStdout', text: probeStdout} : null,
			result.stderr ? {name: 'probeStderr', text: trimText(result.stderr)} : null,
		],
	});
}

function loadNativeBinding({moduleName, nativePath, nativeRoot, packageDir, skipNativeProbeEnv, probe = true}) {
	if (!existsSync(nativePath)) {
		return {
			binding: null,
			loadError: createNativeLoadError({
				moduleName,
				nativePath,
				nativeRoot,
				packageDir,
				reason: 'native binary not found',
				skipNativeProbeEnv,
			}),
		};
	}
	const nativeProbeError = probe
		? probeNativeBinary({moduleName, nativePath, nativeRoot, packageDir, skipNativeProbeEnv})
		: null;
	if (nativeProbeError) {
		return {binding: null, loadError: nativeProbeError};
	}
	try {
		return {binding: require(nativePath), loadError: null};
	} catch (error) {
		return {
			binding: null,
			loadError: createNativeLoadError({
				moduleName,
				nativePath,
				nativeRoot,
				packageDir,
				reason: 'require(nativePath) threw',
				cause: error,
				skipNativeProbeEnv,
			}),
		};
	}
}

module.exports = {
	collectNativeDiagnostics,
	createNativeLoadError,
	formatNativeDiagnostics,
	isNativeLoadError,
	loadNativeBinding,
	probeNativeBinary,
};
