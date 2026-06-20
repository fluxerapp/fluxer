// SPDX-License-Identifier: AGPL-3.0-or-later

import {createConnection, createServer, type Server} from 'node:net';
import {join} from 'node:path';
import {unlinkSync} from 'node:fs';
import {EventEmitter} from 'node:events';
import log from 'electron-log';
import {resolveByClientId} from '@electron/main/DetectableApplications';
import {
	ACTIVITY_FLAG_INSTANCE,
	ActivityType,
	IPCErrorCode,
	IPCMessageType,
	IPC_SOCKET_NAME,
	IPC_MAX_RETRIES,
	IPCCloseCode,
	RPCCommand,
	RPCEvent,
	RPC_PROTOCOL_VERSION,
} from '@electron/main/rpc/RpcConstants';
import type {ExtendedSocket, RPCMessage, RpcActivityPayload, SetActivityArgs} from '@electron/main/rpc/RpcTypes';
import {encodeIpcMessage, getUnixSocketBaseDir, normalizeTimestamps, resolveRpcActivityName} from '@electron/main/rpc/RpcUtils';

const RPC_GENERIC_ERROR = 1000;
type RpcActivityEventSource = 'ipc' | 'ipc-clear' | 'ipc-disconnect' | 'process-scan';

let ipcServer: Server | null = null;
let socketPath: string | null = null;
let socketIdCounter = 0;
let connectedIpcClients = 0;
const activityEmitter = new EventEmitter();

export function getConnectedIpcClientCount(): number {
	return connectedIpcClients;
}

export function onRpcActivity(
	listener: (activity: RpcActivityPayload | null, pid?: number, source?: RpcActivityEventSource) => void,
): () => void {
	activityEmitter.on('activity', listener);
	return () => activityEmitter.off('activity', listener);
}

export function onIpcClientCountChange(listener: (count: number) => void): () => void {
	activityEmitter.on('ipc-clients-changed', listener);
	return () => activityEmitter.off('ipc-clients-changed', listener);
}

function notifyIpcClientCountChanged(): void {
	activityEmitter.emit('ipc-clients-changed', connectedIpcClients);
}

function readSocket(socket: ExtendedSocket): void {
	while (true) {
		const chunk = socket.read() as Buffer | null;
		if (chunk === null) break;
		socket._readBuffer = socket._readBuffer ? Buffer.concat([socket._readBuffer, chunk]) : chunk;
	}

	let buffer = socket._readBuffer ?? Buffer.alloc(0);
	while (buffer.length >= 8) {
		const type = buffer.readInt32LE(0);
		const dataSize = buffer.readInt32LE(4);
		if (type < 0 || type > 5) throw new Error('invalid type');
		if (dataSize < 0 || dataSize > 1024 * 1024) throw new Error(`payload too large: ${dataSize}`);
		const frameEnd = 8 + dataSize;
		if (buffer.length < frameEnd) break;
		const data = buffer.subarray(8, frameEnd);
		buffer = buffer.subarray(frameEnd);
		const parsed = JSON.parse(data.toString()) as unknown;
		switch (type) {
			case IPCMessageType.PING:
				socket.write(encodeIpcMessage(IPCMessageType.PONG, parsed));
				break;
			case IPCMessageType.PONG:
				break;
			case IPCMessageType.HANDSHAKE:
				if (socket._handshook) throw new Error('already handshook');
				socket._handshook = true;
				handleHandshake(socket, parsed as {v?: string; client_id?: string});
				break;
			case IPCMessageType.FRAME:
				if (!socket._handshook) throw new Error('need to handshake first');
				handleFrame(socket, parsed as RPCMessage);
				break;
			case IPCMessageType.CLOSE:
				socket._readBuffer = undefined;
				socket.end();
				socket.destroy();
				return;
		}
	}
	socket._readBuffer = buffer.length > 0 ? buffer : undefined;
}

function closeSocket(socket: ExtendedSocket, code: IPCCloseCode | IPCErrorCode = IPCCloseCode.CLOSE_NORMAL, message = ''): void {
	socket.write(encodeIpcMessage(IPCMessageType.CLOSE, {code, message}));
	socket.end();
	socket.destroy();
}

function sendFrame(socket: ExtendedSocket, msg: RPCMessage): void {
	socket.write(encodeIpcMessage(IPCMessageType.FRAME, msg));
}

function emitActivity(
	activity: RpcActivityPayload | null,
	pid?: number,
	source: RpcActivityEventSource = 'ipc',
): void {
	activityEmitter.emit('activity', activity, pid, source);
}

function handleConnectionClose(socket: ExtendedSocket): void {
	if (socket._handshook) {
		connectedIpcClients = Math.max(0, connectedIpcClients - 1);
		notifyIpcClientCountChanged();
	}
	if (connectedIpcClients === 0) {
		emitActivity(null, socket.lastPid, 'ipc-disconnect');
	}
}

function handleHandshake(socket: ExtendedSocket, params: {v?: string; client_id?: string}): void {
	const ver = Number.parseInt(params.v ?? String(RPC_PROTOCOL_VERSION), 10);
	const clientId = params.client_id ?? '';
	if (ver !== RPC_PROTOCOL_VERSION) {
		closeSocket(socket, IPCErrorCode.INVALID_VERSION);
		return;
	}
	if (!clientId) {
		closeSocket(socket, IPCErrorCode.INVALID_CLIENTID);
		return;
	}

	socket.clientId = clientId;
	socket.clientName = resolveByClientId(clientId)?.name ?? '';
	socket.send = (msg) => sendFrame(socket, msg);
	connectedIpcClients += 1;
	notifyIpcClientCountChanged();

	let closed = false;
	const onClose = () => {
		if (closed) return;
		closed = true;
		handleConnectionClose(socket);
	};
	socket.on('error', onClose);
	socket.on('end', onClose);
	socket.on('close', onClose);

	sendFrame(socket, {
		cmd: RPCCommand.DISPATCH,
		evt: RPCEvent.READY,
		data: {
			v: RPC_PROTOCOL_VERSION,
			config: {
				cdn_host: '',
				api_endpoint: '//fluxer.app/api',
				environment: 'production',
			},
			user: {
				id: '0',
				username: 'fluxer',
				discriminator: '0',
				global_name: 'Fluxer',
				avatar: null,
				bot: false,
			},
		},
		nonce: null,
	});

	const resolved = resolveByClientId(clientId);
	if (resolved) socket.clientName = resolved.name;
	log.info('[RPC] IPC handshake', {clientId, name: socket.clientName});
}

function handleSetActivity(socket: ExtendedSocket, msg: RPCMessage): void {
	const args = msg.args as SetActivityArgs | undefined;
	const activity = args?.activity ?? null;
	const pid = args?.pid;
	socket.lastPid = pid ?? socket.lastPid;

	if (!activity) {
		sendFrame(socket, {cmd: RPCCommand.SET_ACTIVITY, data: null, evt: null, nonce: msg.nonce ?? null});
		emitActivity(null, pid, 'ipc-clear');
		return;
	}

	const normalizedActivity = {...activity} as Record<string, unknown> & {
		timestamps?: Record<string, unknown>;
		instance?: boolean;
		name?: string;
		type?: number;
	};
	const timestamps = normalizedActivity.timestamps;
	normalizeTimestamps(timestamps);
	const resolved = socket.clientId ? resolveByClientId(socket.clientId) : null;
	const appName = socket.clientName || resolved?.name || 'Unknown';
	const type = typeof normalizedActivity.type === 'number' ? normalizedActivity.type : ActivityType.PLAYING;
	const details = typeof normalizedActivity.details === 'string' ? normalizedActivity.details : undefined;
	const state = typeof normalizedActivity.state === 'string' ? normalizedActivity.state : undefined;
	const rawName =
		typeof normalizedActivity.name === 'string' && normalizedActivity.name.length > 0
			? normalizedActivity.name
			: undefined;
	const name = resolveRpcActivityName(appName, rawName, details, state);
	const payload: RpcActivityPayload = {
		application_id: socket.clientId ?? '',
		name,
		type,
		flags: normalizedActivity.instance ? ACTIVITY_FLAG_INSTANCE : 0,
		...(normalizedActivity as Omit<RpcActivityPayload, 'application_id' | 'name' | 'type' | 'flags'>),
		...(timestamps ? {timestamps: timestamps as RpcActivityPayload['timestamps']} : {}),
		pid,
	};
	sendFrame(socket, {
		cmd: RPCCommand.SET_ACTIVITY,
		data: {
			...payload,
		},
		evt: null,
		nonce: msg.nonce ?? null,
	});
	emitActivity(payload, pid);
	log.info('[RPC] SET_ACTIVITY', {name: payload.name, details: payload.details, state: payload.state});
}

function handleFrame(socket: ExtendedSocket, msg: RPCMessage): void {
	if (!msg?.cmd) return;
	switch (msg.cmd) {
		case RPCCommand.SET_ACTIVITY:
			handleSetActivity(socket, msg);
			break;
		case RPCCommand.SUBSCRIBE:
		case RPCCommand.UNSUBSCRIBE:
			sendFrame(socket, {cmd: msg.cmd, data: {evt: msg.evt ?? null}, evt: null, nonce: msg.nonce ?? null});
			break;
		default:
			sendFrame(socket, {
				cmd: msg.cmd,
				data: {code: RPC_GENERIC_ERROR, message: `Unknown command: ${msg.cmd}`},
				evt: RPCEvent.ERROR,
				nonce: msg.nonce ?? null,
			});
	}
}

function attachReadable(socket: ExtendedSocket): void {
	socket.pause();
	socket.on('readable', () => {
		try {
			readSocket(socket);
		} catch (error) {
			log.warn('[RPC] IPC read error', error);
			closeSocket(socket, IPCCloseCode.CLOSE_UNSUPPORTED, error instanceof Error ? error.message : 'read error');
		}
	});
}

async function getAvailableSocketPath(tries = 0): Promise<string> {
	if (tries > IPC_MAX_RETRIES) throw new Error('ran out of IPC socket tries');
	const candidate = `${join(getUnixSocketBaseDir(), IPC_SOCKET_NAME)}-${tries}`;
	const available = await new Promise<boolean>((resolve) => {
		const probe = createConnection(candidate);
		probe.once('connect', () => {
			probe.end();
			probe.destroy();
			resolve(false);
		});
		probe.once('error', () => resolve(true));
	});
	if (available) {
		try {
			unlinkSync(candidate);
		} catch {}
		return candidate;
	}
	return getAvailableSocketPath(tries + 1);
}

export async function startArRpcServer(): Promise<void> {
	if (process.platform !== 'linux') {
		log.info('[RPC] ArRpcServer skipped (linux-only for now)');
		return;
	}
	if (ipcServer) return;
	const path = await getAvailableSocketPath();
	socketPath = path;
	ipcServer = createServer((socket) => {
		const ext = socket as ExtendedSocket;
		ext.socketId = socketIdCounter++;
		attachReadable(ext);
	});
	await new Promise<void>((resolve, reject) => {
		ipcServer!.listen(path, () => {
			log.info('[RPC] ArRpcServer listening at', path);
			resolve();
		});
		ipcServer!.once('error', reject);
	});
}

export async function stopArRpcServer(): Promise<void> {
	if (!ipcServer) return;
	const server = ipcServer;
	const path = socketPath;
	ipcServer = null;
	socketPath = null;
	await new Promise<void>((resolve) => server.close(() => resolve()));
	if (path) {
		try {
			unlinkSync(path);
		} catch {}
	}
}

export function emitSyntheticActivity(activity: RpcActivityPayload | null, pid?: number): void {
	emitActivity(activity, pid, 'process-scan');
}
