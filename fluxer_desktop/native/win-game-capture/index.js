// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');

const MODULE_NAME = '@fluxer/win-game-capture';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName(arch) {
	switch (arch) {
		case 'x64':
			return 'win-game-capture.win32-x64-msvc.node';
		case 'arm64':
			return 'win-game-capture.win32-arm64-msvc.node';
		default:
			return null;
	}
}

let binding = null;
let loadError = null;
const nativeRoot = resolveNativeRoot();

if (process.platform === 'win32') {
	const fileName = nativeFileName(process.arch);
	if (!fileName) {
		loadError = createNativeLoadError({
			moduleName: MODULE_NAME,
			nativeRoot,
			packageDir: __dirname,
			reason: `unsupported Windows architecture: ${process.arch}`,
		});
	} else {
		const nativePath = join(nativeRoot, fileName);
		const loaded = loadNativeBinding({
			moduleName: MODULE_NAME,
			nativePath,
			nativeRoot,
			packageDir: __dirname,
			probe: false,
		});
		binding = loaded.binding;
		loadError = loaded.loadError;
	}
} else {
	loadError = createNativeLoadError({
		moduleName: MODULE_NAME,
		nativeRoot,
		packageDir: __dirname,
		reason: `not supported on platform ${process.platform}`,
	});
}

function normalizeProcessId(processId) {
	if (processId === undefined || processId === null) return undefined;
	if (Number.isInteger(processId) && processId > 0) return processId;
	throw new TypeError(`Invalid process id: ${processId}`);
}

function normalizePriorityClass(priorityClass) {
	if (priorityClass === undefined || priorityClass === null) return undefined;
	const normalized = String(priorityClass).trim().toLowerCase();
	if (normalized === 'real-time') return 'realtime';
	if (normalized === 'high' || normalized === 'realtime') return normalized;
	throw new TypeError(`Invalid GPU scheduling priority class: ${priorityClass}`);
}

function elevateGpuSchedulingPriority(processId, priorityClass) {
	if (!binding) return false;
	try {
		binding.elevateGpuSchedulingPriority(normalizeProcessId(processId), normalizePriorityClass(priorityClass));
		return true;
	} catch (error) {
		console.warn('[win-game-capture] elevateGpuSchedulingPriority failed:', error?.message || error);
		return false;
	}
}

function restoreGpuSchedulingPriority(processId) {
	if (!binding) return false;
	try {
		binding.restoreGpuSchedulingPriority(normalizeProcessId(processId));
		return true;
	} catch (error) {
		console.warn('[win-game-capture] restoreGpuSchedulingPriority failed:', error?.message || error);
		return false;
	}
}

module.exports = {
	elevateGpuSchedulingPriority,
	restoreGpuSchedulingPriority,
	get loadError() {
		return loadError;
	},
};
