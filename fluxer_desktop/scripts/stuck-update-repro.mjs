import fs from 'node:fs';
import http from 'node:http';
import https from 'node:https';
import path from 'node:path';
import {spawn} from 'node:child_process';

const [mode, variant, arch, channel, version, fromVersion] = process.argv.slice(2);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const installRoot = path.join(process.env.LOCALAPPDATA, channel === 'canary' ? 'fluxer_desktop_canary' : 'fluxer_desktop');
const dataRoot = path.join(process.env.APPDATA, channel === 'canary' ? 'fluxercanary' : 'fluxer');
const base = `https://pkgs.fluxer.com/desktop/${channel}/win32/${arch}`;

function exePath() {
	const dir = path.join(installRoot, 'current');
	const name = fs.readdirSync(dir).find((f) => /^Fluxer.*\.exe$/i.test(f) && !f.includes('ExecutionStub'));
	if (!name) throw new Error('no exe');
	return path.join(dir, name);
}
function installed() {
	try {
		return fs.readFileSync(path.join(installRoot, 'current', 'sq.version'), 'utf8').trim();
	} catch {
		return null;
	}
}
function readState() {
	try {
		return JSON.parse(fs.readFileSync(path.join(dataRoot, 'modules', 'state.json'), 'utf8'));
	} catch {
		return null;
	}
}
async function fetchJson(url) {
	return (await fetch(url, {cache: 'no-store'})).json();
}
function launch(args, env) {
	const child = spawn(exePath(), args, {env: {...process.env, ...env}, detached: true, stdio: 'ignore'});
	child.unref();
	return child;
}
function killAll() {
	spawn('powershell', ['-NoProfile', '-Command', `Get-Process | Where-Object { $_.Path -like "${installRoot}*" } | Stop-Process -Force -ErrorAction SilentlyContinue`], {stdio: 'inherit'});
}

async function seed() {
	const live = await fetchJson(`${base}/modules.json`);
	const rendererSha = live.modules.fluxer_renderer.sha256;
	const mirror = structuredClone(live);
	mirror.shell.latest_version = fromVersion;
	mirror.metadata_version = live.metadata_version - 1;
	const body = Buffer.from(JSON.stringify(mirror));
	const server = http.createServer((req, res) => {
		if (req.url.endsWith(`/desktop/${channel}/win32/${arch}/modules.json`)) {
			res.writeHead(200, {'content-type': 'application/json', 'cache-control': 'no-store'});
			res.end(body);
			return;
		}
		https.get(`https://pkgs.fluxer.com${req.url}`, (up) => {
			res.writeHead(up.statusCode, up.headers);
			up.pipe(res);
		}).on('error', () => {
			res.writeHead(502);
			res.end();
		});
	});
	await new Promise((r) => server.listen(8099, '127.0.0.1', r));
	launch(['--disable-gpu'], {FLUXER_DESKTOP_PACKAGE_ORIGIN: 'http://127.0.0.1:8099'});
	const deadline = Date.now() + 8 * 60_000;
	while (Date.now() < deadline) {
		await sleep(5000);
		const state = readState();
		if (state?.committed?.fluxer_renderer === rendererSha) break;
	}
	await sleep(20_000);
	killAll();
	await sleep(5000);
	server.close();
	const state = readState();
	console.log('seeded state', JSON.stringify({shell: state?.shell_version, committed: state?.committed, installed: installed()}));
	if (state?.committed?.fluxer_renderer !== rendererSha) throw new Error('renderer 45608 was not committed');
	if (variant === 'sticky') {
		fs.writeFileSync(path.join(dataRoot, 'update-apply-state.json'), JSON.stringify({version: '2026.1009.20411', attemptedAt: Date.now() - 3_600_000}));
		console.log('wrote update-apply-state.json for 2026.1009.20411');
	}
}

async function cdpEval(expression) {
	const targets = await (await fetch('http://127.0.0.1:9333/json/list')).json();
	const page = targets.find((t) => t.type === 'page' && t.url.startsWith('fluxer-app://app'));
	if (!page) return {error: 'no app page', targets: targets.map((t) => t.url)};
	const socket = new WebSocket(page.webSocketDebuggerUrl);
	await new Promise((r, j) => {
		socket.onopen = r;
		socket.onerror = j;
	});
	const result = await new Promise((resolve) => {
		socket.onmessage = (event) => resolve(JSON.parse(event.data));
		socket.send(JSON.stringify({id: 1, method: 'Runtime.evaluate', params: {expression, awaitPromise: true, returnByValue: true}}));
		setTimeout(() => resolve({timeout: true}), 15_000);
	});
	socket.close();
	return result;
}

async function click() {
	launch(['--disable-gpu', '--remote-debugging-port=9333'], {});
	let state = null;
	for (let i = 0; i < 60; i++) {
		await sleep(5000);
		try {
			const r = await cdpEval('window.electron?.desktopUpdate ? window.electron.desktopUpdate.state() : "no api"');
			state = r?.result?.result?.value;
			if (state && typeof state === 'object' && state.available) break;
		} catch {}
	}
	console.log('update state before click', JSON.stringify(state));
	const started = await cdpEval('(window.electron.desktopUpdate.start(), "started")').catch((e) => ({error: String(e)}));
	console.log('start', JSON.stringify(started?.result?.result?.value ?? started));
	const deadline = Date.now() + 6 * 60_000;
	while (Date.now() < deadline) {
		await sleep(10_000);
		if (installed() === version) break;
	}
	await sleep(30_000);
	console.log('installed after click', installed());
	killAll();
}

if (mode === 'seed') await seed();
else if (mode === 'click') await click();
