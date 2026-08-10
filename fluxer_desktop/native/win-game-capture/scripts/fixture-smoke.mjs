#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawn} from 'node:child_process';
import {existsSync} from 'node:fs';
import {createRequire} from 'node:module';
import {dirname, join} from 'node:path';
import {setTimeout as delay} from 'node:timers/promises';
import {fileURLToPath} from 'node:url';

const require = createRequire(import.meta.url);
const capture = require('../index.js');

const ROOT = dirname(dirname(fileURLToPath(import.meta.url)));
const DEFAULT_FIXTURES = ['d3d11-present-fixture'];
const ALL_FIXTURES = [
	'd3d9-present-fixture',
	'd3d10-present-fixture',
	'd3d11-present-fixture',
	'd3d12-present-fixture',
	'opengl-swapbuffers-fixture',
	'vulkan-present-fixture',
];
const WIDTH = Number.parseInt(process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURE_WIDTH ?? '640', 10);
const HEIGHT = Number.parseInt(process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURE_HEIGHT ?? '360', 10);
const FRAME_RATE = Number.parseInt(process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURE_FPS ?? '30', 10);
const START_TIMEOUT_MS = Number.parseInt(process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURE_START_TIMEOUT_MS ?? '15000', 10);
const FRAME_TIMEOUT_MS = Number.parseInt(process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURE_FRAME_TIMEOUT_MS ?? '15000', 10);

const MIN_OBSERVED_FRAMES = 3;

const API_NONE = 0;
const TRANSPORT_MEMORY = 0;
const FALLBACK_NONE = 0;
const DXGI_FORMAT_UNKNOWN = 0;
const EXPECTED_ACTIVE_STRATEGY = 'wgc';

const EXPECTED_DIAGNOSTICS = {
	apiType: API_NONE,
	transport: TRANSPORT_MEMORY,
	fallbackReason: FALLBACK_NONE,
	dxgiFormat: DXGI_FORMAT_UNKNOWN,
};

function envFlag(name) {
	return /^(1|true|yes|on)$/i.test(process.env[name] ?? '');
}

function selectedFixtures() {
	const raw = process.argv.slice(2).join(',') || process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURES || '';
	if (!raw.trim()) return DEFAULT_FIXTURES;
	const names = raw
		.split(',')
		.map((name) => name.trim())
		.filter(Boolean);
	return names.flatMap((name) => (name === 'all' ? ALL_FIXTURES : [name]));
}

function fixtureTarget(fixture) {
	return fixture === 'i686-present-fixture' ? 'i686-pc-windows-msvc' : null;
}

function fixtureExePath(fixture) {
	const target = fixtureTarget(fixture);
	const targetDir = target
		? join(ROOT, 'test-apps', fixture, 'target', target, 'release')
		: join(ROOT, 'test-apps', fixture, 'target', 'release');
	return join(targetDir, `${fixture}.exe`);
}

function envKeyForFixture(fixture) {
	return `FLUXER_WIN_GAME_CAPTURE_FIXTURE_ARGS_${fixture.toUpperCase().replaceAll(/[^A-Z0-9]+/g, '_')}`;
}

function fixtureEnvValue(baseName, fixture) {
	const fixtureKey = `${baseName}_${fixture.toUpperCase().replaceAll(/[^A-Z0-9]+/g, '_')}`;
	return process.env[fixtureKey] ?? process.env[baseName];
}

function splitExtraArgs(raw) {
	return (raw ?? '')
		.split(/\s+/)
		.map((arg) => arg.trim())
		.filter(Boolean);
}

function extraFixtureArgs(fixture) {
	return [
		...splitExtraArgs(process.env.FLUXER_WIN_GAME_CAPTURE_FIXTURE_ARGS),
		...splitExtraArgs(process.env[envKeyForFixture(fixture)]),
	];
}

function diagnosticOverride(baseName, fixture) {
	const raw = fixtureEnvValue(baseName, fixture);
	if (raw === undefined || raw === '') return undefined;
	const value = Number.parseInt(raw, 10);
	if (!Number.isFinite(value)) throw new Error(`Invalid ${baseName} override: ${raw}`);
	return value;
}

function runCommand(command, args, options = {}) {
	return new Promise((resolve, reject) => {
		const child = spawn(command, args, {
			cwd: options.cwd ?? ROOT,
			env: process.env,
			stdio: options.stdio ?? ['ignore', 'pipe', 'pipe'],
			windowsHide: false,
		});
		let stdout = '';
		let stderr = '';
		child.stdout?.on('data', (chunk) => {
			stdout += chunk.toString();
			if (options.echo) process.stdout.write(chunk);
		});
		child.stderr?.on('data', (chunk) => {
			stderr += chunk.toString();
			if (options.echo) process.stderr.write(chunk);
		});
		child.once('error', reject);
		child.once('exit', (code, signal) => {
			if (code === 0) {
				resolve({stdout, stderr});
				return;
			}
			reject(new Error(`${command} ${args.join(' ')} failed with ${signal ?? code}\n${stderr || stdout}`));
		});
	});
}

async function buildFixture(fixture) {
	const manifest = join(ROOT, 'test-apps', fixture, 'Cargo.toml');
	if (!existsSync(manifest)) throw new Error(`unknown fixture: ${fixture}`);
	const exe = fixtureExePath(fixture);
	if (envFlag('FLUXER_WIN_GAME_CAPTURE_FIXTURE_SKIP_BUILD') && existsSync(exe)) {
		console.log(`[fixture-smoke] using existing ${fixture}`);
		return exe;
	}
	const args = ['build', '--release', '--manifest-path', manifest];
	const target = fixtureTarget(fixture);
	if (target) args.push('--target', target);
	console.log(`[fixture-smoke] building ${fixture}`);
	await runCommand('cargo', args, {echo: envFlag('FLUXER_WIN_GAME_CAPTURE_FIXTURE_VERBOSE')});
	if (!existsSync(exe)) throw new Error(`fixture build did not produce ${exe}`);
	return exe;
}

function waitForHwnd(child, fixture) {
	return new Promise((resolve, reject) => {
		let stdout = '';
		let stderr = '';
		const timeout = setTimeout(() => {
			reject(new Error(`${fixture} did not print HWND within ${START_TIMEOUT_MS}ms\n${stderr || stdout}`));
		}, START_TIMEOUT_MS);
		const finish = (hwnd) => {
			clearTimeout(timeout);
			resolve(hwnd);
		};
		child.stdout.on('data', (chunk) => {
			const text = chunk.toString();
			stdout += text;
			const match = stdout.match(/HWND=(\d+)/);
			if (match) finish(match[1]);
		});
		child.stderr.on('data', (chunk) => {
			stderr += chunk.toString();
			if (envFlag('FLUXER_WIN_GAME_CAPTURE_FIXTURE_VERBOSE')) process.stderr.write(chunk);
		});
		child.once('exit', (code, signal) => {
			clearTimeout(timeout);
			reject(new Error(`${fixture} exited before capture started (${signal ?? code})\n${stderr || stdout}`));
		});
	});
}

function observedFrameCount(diagnostics) {
	if (!diagnostics) return 0;
	const accepted = Number(diagnostics.frameSinkAccepted ?? 0);
	const coalesced = Number(diagnostics.frameSinkCoalesced ?? 0);
	const droppedWithoutSink = Number(diagnostics.mediaFramesDroppedWithoutSink ?? 0);
	const total = accepted + coalesced + droppedWithoutSink;
	return Number.isFinite(total) ? total : 0;
}

async function waitForAdvancingFrames(screenCapture, fixture, stalls) {
	let lastDiagnostics = null;
	const start = Date.now();
	while (Date.now() - start < FRAME_TIMEOUT_MS) {
		lastDiagnostics = screenCapture.getDiagnostics?.() ?? lastDiagnostics;
		const frameCount = observedFrameCount(lastDiagnostics);
		if (frameCount >= MIN_OBSERVED_FRAMES) {
			return {frameCount, diagnostics: lastDiagnostics};
		}
		await delay(100);
	}
	throw new Error(
		`${fixture} capture did not deliver advancing frames within ${FRAME_TIMEOUT_MS}ms (frames=${observedFrameCount(lastDiagnostics)}, stalls=${JSON.stringify(stalls)}, diagnostics=${JSON.stringify(lastDiagnostics)})`,
	);
}

function assertEqualDiagnostic(fixture, diagnostics, key, expected) {
	if (diagnostics?.[key] !== expected) {
		throw new Error(`${fixture} expected diagnostics.${key}=${expected}, got ${JSON.stringify(diagnostics)}`);
	}
}

function assertFixtureDiagnostics(fixture, diagnostics) {
	const expected = {...EXPECTED_DIAGNOSTICS};
	const apiOverride = diagnosticOverride('FLUXER_WIN_GAME_CAPTURE_EXPECT_API_TYPE', fixture);
	const transportOverride = diagnosticOverride('FLUXER_WIN_GAME_CAPTURE_EXPECT_TRANSPORT', fixture);
	const fallbackOverride = diagnosticOverride('FLUXER_WIN_GAME_CAPTURE_EXPECT_FALLBACK_REASON', fixture);
	if (apiOverride !== undefined) expected.apiType = apiOverride;
	if (transportOverride !== undefined) expected.transport = transportOverride;
	if (fallbackOverride !== undefined) expected.fallbackReason = fallbackOverride;

	if (diagnostics?.activeStrategy !== EXPECTED_ACTIVE_STRATEGY) {
		throw new Error(
			`${fixture} expected activeStrategy=${EXPECTED_ACTIVE_STRATEGY}, got ${JSON.stringify(diagnostics)}`,
		);
	}
	assertEqualDiagnostic(fixture, diagnostics, 'apiType', expected.apiType);
	assertEqualDiagnostic(fixture, diagnostics, 'transport', expected.transport);
	assertEqualDiagnostic(fixture, diagnostics, 'fallbackReason', expected.fallbackReason);
	assertEqualDiagnostic(fixture, diagnostics, 'dxgiFormat', expected.dxgiFormat);
	assertEqualDiagnostic(fixture, diagnostics, 'injectionMethod', '');
}

async function runFixture(fixture) {
	const exe = await buildFixture(fixture);
	const args = ['--frames', '900', '--width', String(WIDTH), '--height', String(HEIGHT), '--windowed'];
	if (fixture === 'vulkan-present-fixture') args.push('--resize-at', '120');
	args.push(...extraFixtureArgs(fixture));
	const child = spawn(exe, args, {
		cwd: dirname(exe),
		stdio: ['ignore', 'pipe', 'pipe'],
		windowsHide: false,
	});
	let started = false;
	try {
		const hwnd = await waitForHwnd(child, fixture);
		const screenCapture = new capture.ScreenCapture({
			sourceId: `window:${hwnd}:0`,
			sourceKind: 'game',
			width: WIDTH,
			height: HEIGHT,
			frameRate: FRAME_RATE,
			injectionMethod: process.env.FLUXER_WIN_GAME_CAPTURE_INJECTION_METHOD || 'auto',
		});
		const stalls = [];
		screenCapture.on('stalled', (message) => stalls.push(message ?? ''));
		let result;
		try {
			result = await screenCapture.start();
			started = true;
			const observed = await waitForAdvancingFrames(screenCapture, fixture, stalls);
			assertFixtureDiagnostics(fixture, observed.diagnostics);
			console.log(
				`[fixture-smoke] PASS ${fixture}: start=${JSON.stringify(result)} frames=${observed.frameCount} stalls=${JSON.stringify(stalls)} diagnostics=${JSON.stringify(observed.diagnostics)}`,
			);
		} finally {
			if (started) await screenCapture.stop().catch(() => {});
		}
	} finally {
		if (!child.killed) child.kill();
	}
}

async function main() {
	if (process.platform !== 'win32') {
		console.log('[fixture-smoke] SKIP: Windows game-capture fixtures only run on Windows');
		return 0;
	}
	if (!capture.isSupported()) {
		throw new Error(`win-game-capture binding unavailable: ${capture.loadError?.message ?? 'unknown error'}`);
	}
	for (const fixture of selectedFixtures()) {
		await runFixture(fixture);
	}
	return 0;
}

try {
	process.exitCode = await main();
} catch (error) {
	console.error(`[fixture-smoke] FAIL: ${error.stack ?? error.message}`);
	process.exitCode = 1;
}
