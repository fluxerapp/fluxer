// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import fs from 'node:fs';
import net from 'node:net';
import os from 'node:os';
import path from 'node:path';
import {promisify} from 'node:util';
import type {RichPresenceActivity} from '@electron/common/Types';
import {BrowserWindow, ipcMain} from 'electron';
import log from 'electron-log';

const execFileAsync = promisify(execFile);
const IPC_NAMES = ['discord-ipc', 'airpc'] as const;
const AIRPC_SLOTS = IPC_NAMES.flatMap((name) => [name, ...Array.from({length: 10}, (_v, idx) => `${name}-${idx}`)]);
const SCAN_MS = 5000;
const MAX_FRAME_BYTES = 64 * 1024;
const MAX_ACTIVITIES = 5;
const OP_HANDSHAKE = 0;
const OP_FRAME = 1;
const OP_PING = 3;
const OP_PONG = 4;

const KNOWN: Record<string, {name: string; type: number}> = {
	spotify: {name: 'Spotify', type: 2},
	'spotify.exe': {name: 'Spotify', type: 2},
	vlc: {name: 'VLC', type: 3},
	'vlc.exe': {name: 'VLC', type: 3},
	mpv: {name: 'mpv', type: 3},
	'mpv.exe': {name: 'mpv', type: 3},
	foobar2000: {name: 'foobar2000', type: 2},
	'foobar2000.exe': {name: 'foobar2000', type: 2},
	deezer: {name: 'Deezer', type: 2},
	'deezer.exe': {name: 'Deezer', type: 2},
	tidal: {name: 'TIDAL', type: 2},
	'tidal.exe': {name: 'TIDAL', type: 2},
	cs2: {name: 'Counter-Strike 2', type: 0},
	'cs2.exe': {name: 'Counter-Strike 2', type: 0},
	'valorant.exe': {name: 'VALORANT', type: 0},
	'valorant-win64-shipping.exe': {name: 'VALORANT', type: 0},
	'league of legends.exe': {name: 'League of Legends', type: 0},
	'leagueclient.exe': {name: 'League of Legends', type: 0},
	'robloxplayerbeta.exe': {name: 'Roblox', type: 0},
	'minecraft.exe': {name: 'Minecraft', type: 0},
	'minecraft.windows.exe': {name: 'Minecraft', type: 0},
	'fortniteclient-win64-shipping.exe': {name: 'Fortnite', type: 0},
	'osu!.exe': {name: 'osu!', type: 0},
	'r5apex.exe': {name: 'Apex Legends', type: 0},
	'overwatch.exe': {name: 'Overwatch', type: 0},
	'rocketleague.exe': {name: 'Rocket League', type: 0},
	'ff14_dx11.exe': {name: 'FINAL FANTASY XIV', type: 0},
	'wow.exe': {name: 'World of Warcraft', type: 0},
	'dota2.exe': {name: 'Dota 2', type: 0},
	'gta5.exe': {name: 'Grand Theft Auto V', type: 0},
	'eldenring.exe': {name: 'ELDEN RING', type: 0},
	'helldivers2.exe': {name: 'HELLDIVERS 2', type: 0},
};

interface ClientState {
	socket: net.Socket;
	buffer: Buffer;
	ready: boolean;
	activity: RichPresenceActivity | null;
}

let started = false;
let scanTimer: NodeJS.Timeout | null = null;
let detected: Array<RichPresenceActivity> = [];
let published = '';
const servers: Array<net.Server> = [];
const socketPaths: Array<string> = [];
const clients = new Set<ClientState>();

export function startRichPresence(): void {
	if (started) return;
	started = true;
	ipcMain.handle('rich-presence:get-activities', () => currentActivities());
	void listenAll();
	void scanProcesses();
	scanTimer = setInterval(() => {
		void scanProcesses();
	}, SCAN_MS);
}

export function stopRichPresence(): void {
	if (!started) return;
	started = false;
	if (scanTimer) {
		clearInterval(scanTimer);
		scanTimer = null;
	}
	ipcMain.removeHandler('rich-presence:get-activities');
	for (const client of clients) client.socket.destroy();
	clients.clear();
	for (const server of servers) server.close();
	servers.length = 0;
	if (process.platform !== 'win32') {
		for (const socketPath of socketPaths) {
			try {
				fs.unlinkSync(socketPath);
			} catch {
				// already gone
			}
		}
	}
	socketPaths.length = 0;
	detected = [];
	published = '';
}

function currentActivities(): Array<RichPresenceActivity> {
	const fromIpc: Array<RichPresenceActivity> = [];
	const names = new Set<string>();
	for (const client of clients) {
		if (!client.activity) continue;
		fromIpc.push(client.activity);
		names.add(client.activity.name.toLowerCase());
	}
	const merged = [...fromIpc];
	for (const activity of detected) {
		if (merged.length >= MAX_ACTIVITIES) break;
		const name = activity.name.toLowerCase();
		if (names.has(name)) continue;
		names.add(name);
		merged.push(activity);
	}
	return merged;
}

function publish(): void {
	if (!started) return;
	const next = currentActivities();
	const key = JSON.stringify(next);
	if (key === published) return;
	published = key;
	for (const win of BrowserWindow.getAllWindows()) {
		if (win.isDestroyed()) continue;
		win.webContents.send('rich-presence:activities', next);
	}
}

function endpoint(slot: string): string {
	if (process.platform === 'win32') return `\\\\.\\pipe\\${slot}`;
	const dir = process.env.XDG_RUNTIME_DIR || os.tmpdir();
	return path.join(dir, `${slot}.sock`);
}

async function listenAll(): Promise<void> {
	for (const slot of AIRPC_SLOTS) {
		if (!started) return;
		const server = await listenSlot(slot);
		if (!started) {
			server?.close();
			return;
		}
		if (server) servers.push(server);
	}
}

async function prepareSocketPath(target: string): Promise<boolean> {
	if (process.platform === 'win32') return true;
	try {
		const stat = fs.lstatSync(target);
		if (!stat.isSocket()) {
			log.warn('[RichPresence] refusing to replace non-socket path', target);
			return false;
		}
	} catch (err) {
		if ((err as NodeJS.ErrnoException).code === 'ENOENT') return true;
		log.warn('[RichPresence] cannot inspect IPC path', target, err);
		return false;
	}
	if (await socketIsActive(target)) return false;
	try {
		fs.unlinkSync(target);
		return true;
	} catch (err) {
		if ((err as NodeJS.ErrnoException).code === 'ENOENT') return true;
		log.warn('[RichPresence] cannot remove stale IPC socket', target, err);
		return false;
	}
}

function socketIsActive(target: string): Promise<boolean> {
	return new Promise((resolve) => {
		const probe = net.createConnection(target);
		let settled = false;
		const finish = (active: boolean): void => {
			if (settled) return;
			settled = true;
			probe.destroy();
			resolve(active);
		};
		probe.setTimeout(250, () => finish(false));
		probe.once('connect', () => finish(true));
		probe.once('error', (err: NodeJS.ErrnoException) => finish(err.code !== 'ECONNREFUSED' && err.code !== 'ENOENT'));
	});
}

function listenSlot(slot: string): Promise<net.Server | null> {
	const target = endpoint(slot);
	return prepareSocketPath(target).then((ready) => {
		if (!ready) return null;
		return new Promise((resolve) => {
			const server = net.createServer((socket) => handleClient(socket));
			let settled = false;
			const finish = (value: net.Server | null): void => {
				if (settled) return;
				settled = true;
				resolve(value);
			};
			server.once('error', (err: Error) => {
				log.warn('[RichPresence] ipc listen failed', slot, err.message);
				server.close();
				finish(null);
			});
			server.listen(target, () => {
				if (!started) {
					server.close();
					finish(null);
					return;
				}
				server.removeAllListeners('error');
				server.on('error', (err) => log.warn('[RichPresence] ipc server error', err));
				if (process.platform !== 'win32') {
					socketPaths.push(target);
					try {
						fs.chmodSync(target, 0o600);
					} catch {
						// best effort
					}
				}
				finish(server);
			});
		});
	});
}

function handleClient(socket: net.Socket): void {
	socket.setNoDelay(true);
	const client: ClientState = {socket, buffer: Buffer.alloc(0), ready: false, activity: null};
	clients.add(client);
	socket.on('data', (chunk) => onData(client, chunk));
	socket.on('error', () => closeClient(client));
	socket.on('close', () => closeClient(client));
}

function closeClient(client: ClientState): void {
	if (!clients.delete(client)) return;
	client.socket.destroy();
	publish();
}

function onData(client: ClientState, chunk: Buffer): void {
	client.buffer = Buffer.concat([client.buffer, chunk]);
	if (client.buffer.length > MAX_FRAME_BYTES * 2) {
		closeClient(client);
		return;
	}
	while (client.buffer.length >= 8) {
		const op = client.buffer.readUInt32LE(0);
		const len = client.buffer.readUInt32LE(4);
		if (len > MAX_FRAME_BYTES) {
			closeClient(client);
			return;
		}
		if (client.buffer.length < 8 + len) return;
		const body = client.buffer.subarray(8, 8 + len);
		client.buffer = client.buffer.subarray(8 + len);
		handleFrame(client, op, body);
		if (!clients.has(client)) return;
	}
}

function handleFrame(client: ClientState, op: number, body: Buffer): void {
	let msg: Record<string, unknown> | null = null;
	if (body.length > 0) {
		try {
			const parsed: unknown = JSON.parse(body.toString('utf8'));
			msg = parsed && typeof parsed === 'object' ? (parsed as Record<string, unknown>) : null;
		} catch {
			closeClient(client);
			return;
		}
	}
	if (op === OP_HANDSHAKE) {
		client.ready = true;
		writeFrame(client.socket, OP_FRAME, {
			cmd: 'DISPATCH',
			evt: 'READY',
			data: {
				v: 1,
				user: {id: '0', username: 'fluxer', discriminator: '0000'},
			},
		});
		return;
	}
	if (op === OP_PING) {
		writeFrame(client.socket, OP_PONG, msg ?? {});
		return;
	}
	if (op !== OP_FRAME || !client.ready || !msg) return;
	const cmd = typeof msg.cmd === 'string' ? msg.cmd : '';
	if (cmd === 'SET_ACTIVITY') {
		const args = msg.args && typeof msg.args === 'object' ? (msg.args as Record<string, unknown>) : {};
		client.activity = normalizeActivity(args.activity);
		publish();
		writeFrame(client.socket, OP_FRAME, {
			cmd: 'SET_ACTIVITY',
			data: client.activity,
			nonce: msg.nonce ?? null,
		});
		return;
	}
	writeFrame(client.socket, OP_FRAME, {cmd, data: null, nonce: msg.nonce ?? null});
}

function writeFrame(socket: net.Socket, op: number, payload: unknown): void {
	const body = Buffer.from(JSON.stringify(payload ?? {}));
	const header = Buffer.alloc(8);
	header.writeUInt32LE(op, 0);
	header.writeUInt32LE(body.length, 4);
	if (!socket.destroyed) socket.write(Buffer.concat([header, body]));
}

function normalizeActivity(raw: unknown): RichPresenceActivity | null {
	if (!raw || typeof raw !== 'object') return null;
	const record = raw as Record<string, unknown>;
	const name = clip(record.name);
	if (!name) return null;
	const type = record.type === 2 || record.type === 3 ? record.type : 0;
	const activity: RichPresenceActivity = {name, type};
	const details = clip(record.details);
	const state = clip(record.state);
	if (details) activity.details = details;
	if (state) activity.state = state;
	if (record.timestamps && typeof record.timestamps === 'object') {
		const ts = record.timestamps as Record<string, unknown>;
		const start = stamp(ts.start);
		const end = stamp(ts.end);
		if (start != null || end != null) {
			activity.timestamps = {
				...(start != null ? {start} : {}),
				...(end != null ? {end} : {}),
			};
		}
	}
	return activity;
}

function clip(raw: unknown): string | null {
	if (typeof raw !== 'string') return null;
	const text = raw.trim();
	if (!text || text.length > 128) return null;
	return text;
}

function stamp(raw: unknown): number | null {
	if (typeof raw !== 'number' || !Number.isFinite(raw) || raw <= 0) return null;
	return Math.trunc(raw);
}

async function scanProcesses(): Promise<void> {
	try {
		const names = process.platform === 'win32' ? await windowsNames() : await posixNames();
		const next: Array<RichPresenceActivity> = [];
		const seen = new Set<string>();
		for (const name of names) {
			const known = KNOWN[name];
			if (!known || seen.has(known.name)) continue;
			seen.add(known.name);
			next.push({name: known.name, type: known.type});
			if (next.length >= MAX_ACTIVITIES) break;
		}
		if (!started) return;
		detected = next;
		publish();
	} catch (err) {
		log.warn('[RichPresence] process scan failed', err);
	}
}

async function windowsNames(): Promise<Array<string>> {
	const {stdout} = await execFileAsync('tasklist', ['/fo', 'csv', '/nh'], {
		windowsHide: true,
		maxBuffer: 1024 * 1024,
	});
	const names: Array<string> = [];
	for (const line of stdout.split(/\r?\n/)) {
		if (!line.trim()) continue;
		const image = csvCell(line);
		if (!image) continue;
		names.push(path.basename(image).toLowerCase());
	}
	return names;
}

function csvCell(line: string): string {
	if (line.startsWith('"')) {
		const end = line.indexOf('"', 1);
		return end > 1 ? line.slice(1, end) : '';
	}
	return line.split(',')[0] ?? '';
}

async function posixNames(): Promise<Array<string>> {
	const {stdout} = await execFileAsync('ps', ['-ax', '-o', 'comm='], {maxBuffer: 1024 * 1024});
	const names: Array<string> = [];
	for (const line of stdout.split(/\r?\n/)) {
		const token = line.trim().split(/\s+/)[0];
		if (!token) continue;
		names.push(path.basename(token).toLowerCase());
	}
	return names;
}
