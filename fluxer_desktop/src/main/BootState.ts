// SPDX-License-Identifier: AGPL-3.0-or-later

import {createCipheriv, createDecipheriv, createHash, randomBytes} from 'node:crypto';
import fsp from 'node:fs/promises';
import path from 'node:path';
import {promisify} from 'node:util';
import {constants as zlibConstants, zstdCompress, zstdDecompress} from 'node:zlib';
import {createChildLogger} from '@electron/common/Logger';
import {writeFileAtomically} from '@electron/main/AtomicFile';
import {registerBootStateIpcRoutes} from '@electron/main/BootStateIpc';
import {hasQuitStarted} from '@electron/main/ModuleBootHandoff';
import {getStreamerModeCaptureAppStatus} from '@electron/main/StreamerModeProcessDetection';
import {
	BOOT_STATE_FILE_VERSION,
	BOOT_STATE_MAX_PAYLOAD_BYTES,
	DESKTOP_BOOT_STATE_CHANNELS,
	type DesktopBootFrame,
	type DesktopBootStateFlushOptions,
	type DesktopBootStateFrameRequest,
	type DesktopBootStateFrameResult,
	type DesktopBootStateReadResult,
	type DesktopBootStateWriteRequest,
	type DesktopBootStateWriteResult,
	readBootStatePayloadFormat,
} from '@fluxer/desktop_ipc/src/BootStateContract';
import {app, BrowserWindow, safeStorage} from 'electron';

const log = createChildLogger('BootState');

const compressAsync = promisify(zstdCompress);
const decompressAsync = promisify(zstdDecompress);

const BOOT_STATE_DIRECTORY = 'boot-state';
const INDEX_PREWARM_WAIT_BUDGET_MS = 300;
const BOOT_STATE_FILE_EXTENSION = '.fxbs';
const BOOT_FRAME_FILE_EXTENSION = '.fxbf';
const BOOT_FRAME_AAD_SUFFIX = '.frame';
const BOOT_FRAME_JPEG_QUALITY = 92;
const BOOT_FRAME_ID_BYTES = 16;
const BOOT_FRAME_ID_OFFSET = 5;
const BOOT_FRAME_VIEWPORT_OFFSET = BOOT_FRAME_ID_OFFSET + BOOT_FRAME_ID_BYTES;
const BOOT_FRAME_HEADER_BYTES = BOOT_FRAME_VIEWPORT_OFFSET + 4;
const BOOT_FRAME_FLAG_CAPTURE_APP_CHECK = 1;
const BOOT_FRAME_FLAG_FOLLOWS_SYSTEM = 2;
const BOOT_FRAME_FLAG_SYSTEM_DARK = 4;
const BOOT_FRAME_KNOWN_FLAGS =
	BOOT_FRAME_FLAG_CAPTURE_APP_CHECK | BOOT_FRAME_FLAG_FOLLOWS_SYSTEM | BOOT_FRAME_FLAG_SYSTEM_DARK;
const CAPTURE_APP_CHECK_BUDGET_MS = 400;
const MAX_BOOT_FRAME_FILE_BYTES = 16 * 1024 * 1024;
const LAST_FOREGROUND_FILE = 'last-foreground';
const TEMPORARY_FILE_SUFFIX = '.tmp';
const ACCOUNT_ID_PREFIX = 'fluxer-boot-state:v1:';
const ACCOUNT_ID_PATTERN = /^[0-9a-f]{40}$/u;
const BOOT_FRAME_FILE_PATTERN = /^([0-9a-f]{40})\.([0-9a-f]{32})\.fxbf$/u;
const ACCOUNT_ID_LENGTH = 40;
const FILE_MODE = 0o600;
const DIRECTORY_MODE = 0o700;

const MAGIC = Buffer.from('FXBS', 'ascii');
const ALGORITHM_ZSTD_AES_256_GCM = 1;
const ALGORITHM_BOOT_FRAME_AES_256_GCM = 4;
const CIPHER = 'aes-256-gcm';
const DATA_KEY_BYTES = 32;
const IV_BYTES = 12;
const TAG_BYTES = 16;
const FIXED_HEADER_BYTES = 10;
const WRITTEN_AT_BYTES = 8;
const MAX_WRAPPED_KEY_BYTES = 0xffff;
const ZSTD_LEVEL = 3;
const MAX_FILE_BYTES = BOOT_STATE_MAX_PAYLOAD_BYTES + 1024 * 1024;

const PREWARM_HOLD_MS = 60_000;
const STALE_FILE_AGE_MS = 30 * 24 * 60 * 60 * 1000;
const STALE_TEMPORARY_FILE_AGE_MS = 60_000;
const MAX_ACCOUNT_FILES = 10;
const MAX_LIVE_TOKENS_PER_ACCOUNT = 32;

type BootStateFlushResult = 'flushed' | 'timed-out' | 'unavailable';

interface DataKey {
	readonly key: Buffer;
	readonly wrapped: Buffer;
}

interface StoredBootFrame {
	readonly frameId: string;
	readonly frame: DesktopBootFrame;
	readonly captureAppCheck: boolean;
}

interface PendingWrite {
	payload: Uint8Array;
	frameId: string | null;
	generation: number;
	readonly waiters: Array<(result: DesktopBootStateWriteResult) => void>;
}

interface AccountLane {
	generation: number;
	readonly tokens: Set<string>;
	dataKey: DataKey | null;
	capturedFrameId: string | null;
	pending: PendingWrite | null;
	tail: Promise<void>;
}

interface LoadedBootState {
	readonly payload: Buffer;
	readonly writtenAt: number;
	readonly frameId: string | null;
}

interface PrewarmedBootFiles {
	readonly accountId: string;
	readonly generation: number;
	readonly state: Buffer | null;
	readonly frames: ReadonlyMap<string, Buffer | null>;
}

interface HeldBootState extends LoadedBootState {
	readonly accountId: string;
	readonly format: string | null;
	readonly timer: NodeJS.Timeout;
	readonly captureAppFree: Promise<boolean> | null;
	frame: StoredBootFrame | null;
	admitted: boolean;
}

interface PendingFlush {
	readonly target: Electron.WebContents;
	readonly settle: (result: BootStateFlushResult) => void;
}

class BootStateCorruptError extends Error {
	public constructor(reason: string) {
		super(`boot state file rejected: ${reason}`);
		this.name = 'BootStateCorruptError';
	}
}

function bootStateAccountId(accountKey: string): string {
	return createHash('sha256')
		.update(`${ACCOUNT_ID_PREFIX}${accountKey}`, 'utf8')
		.digest('hex')
		.slice(0, ACCOUNT_ID_LENGTH);
}

function isEncryptionUsable(): boolean {
	try {
		if (!safeStorage.isEncryptionAvailable()) {
			return false;
		}
		if (process.platform === 'linux') {
			const backend = safeStorage.getSelectedStorageBackend();
			return backend !== 'basic_text' && backend !== 'unknown';
		}
		return true;
	} catch {
		return false;
	}
}

function buildAAD(aadId: string, algorithm: number): Buffer {
	const header = Buffer.alloc(MAGIC.length + 3);
	MAGIC.copy(header, 0);
	header.writeUInt16LE(BOOT_STATE_FILE_VERSION, MAGIC.length);
	header.writeUInt8(algorithm, MAGIC.length + 2);
	return Buffer.concat([header, Buffer.from(aadId, 'ascii')]);
}

function createDataKey(): DataKey {
	const key = randomBytes(DATA_KEY_BYTES);
	const wrapped = safeStorage.encryptString(key.toString('base64'));
	if (wrapped.byteLength === 0 || wrapped.byteLength > MAX_WRAPPED_KEY_BYTES) {
		throw new Error('safeStorage returned an unusable wrapped key');
	}
	return {key, wrapped};
}

function unwrapDataKey(wrapped: Buffer): Buffer {
	const key = Buffer.from(safeStorage.decryptString(wrapped), 'base64');
	if (key.byteLength !== DATA_KEY_BYTES) {
		throw new BootStateCorruptError('wrapped key has the wrong length');
	}
	return key;
}

function encodeContainer(aadId: string, algorithm: number, dataKey: DataKey, body: Buffer, writtenAt: number): Buffer {
	const iv = randomBytes(IV_BYTES);
	const cipher = createCipheriv(CIPHER, dataKey.key, iv, {authTagLength: TAG_BYTES});
	cipher.setAAD(buildAAD(aadId, algorithm));
	const ciphertext = Buffer.concat([cipher.update(body), cipher.final()]);
	const header = Buffer.alloc(FIXED_HEADER_BYTES);
	MAGIC.copy(header, 0);
	header.writeUInt16LE(BOOT_STATE_FILE_VERSION, 4);
	header.writeUInt8(algorithm, 6);
	header.writeUInt8(0, 7);
	header.writeUInt16LE(dataKey.wrapped.byteLength, 8);
	const stamp = Buffer.alloc(WRITTEN_AT_BYTES);
	stamp.writeDoubleLE(writtenAt, 0);
	return Buffer.concat([header, dataKey.wrapped, iv, cipher.getAuthTag(), stamp, ciphertext]);
}

interface DecodedContainer {
	readonly wrapped: Buffer;
	readonly iv: Buffer;
	readonly tag: Buffer;
	readonly writtenAt: number;
	readonly ciphertext: Buffer;
}

function decodeContainer(file: Buffer, algorithm: number): DecodedContainer {
	if (file.byteLength < FIXED_HEADER_BYTES || !file.subarray(0, 4).equals(MAGIC)) {
		throw new BootStateCorruptError('bad magic');
	}
	if (file.readUInt16LE(4) !== BOOT_STATE_FILE_VERSION) {
		throw new BootStateCorruptError('unknown version');
	}
	if (file.readUInt8(6) !== algorithm) {
		throw new BootStateCorruptError('unknown algorithm');
	}
	const wrappedLength = file.readUInt16LE(8);
	const ivOffset = FIXED_HEADER_BYTES + wrappedLength;
	const tagOffset = ivOffset + IV_BYTES;
	const writtenAtOffset = tagOffset + TAG_BYTES;
	const ciphertextOffset = writtenAtOffset + WRITTEN_AT_BYTES;
	if (wrappedLength === 0 || ciphertextOffset > file.byteLength) {
		throw new BootStateCorruptError('truncated header');
	}
	return {
		wrapped: file.subarray(FIXED_HEADER_BYTES, ivOffset),
		iv: file.subarray(ivOffset, tagOffset),
		tag: file.subarray(tagOffset, writtenAtOffset),
		writtenAt: file.readDoubleLE(writtenAtOffset),
		ciphertext: file.subarray(ciphertextOffset),
	};
}

function decryptContainer(aadId: string, algorithm: number, container: DecodedContainer, key: Buffer): Buffer {
	const decipher = createDecipheriv(CIPHER, key, container.iv, {authTagLength: TAG_BYTES});
	decipher.setAAD(buildAAD(aadId, algorithm));
	decipher.setAuthTag(container.tag);
	return Buffer.concat([decipher.update(container.ciphertext), decipher.final()]);
}

function encodeFrameBody(stored: StoredBootFrame): Buffer {
	const header = Buffer.alloc(BOOT_FRAME_HEADER_BYTES);
	header.writeUInt16LE(stored.frame.pixelWidth, 0);
	header.writeUInt16LE(stored.frame.pixelHeight, 2);
	const {systemDark} = stored.frame;
	header.writeUInt8(
		(stored.captureAppCheck ? BOOT_FRAME_FLAG_CAPTURE_APP_CHECK : 0) |
			(systemDark === null ? 0 : BOOT_FRAME_FLAG_FOLLOWS_SYSTEM) |
			(systemDark === true ? BOOT_FRAME_FLAG_SYSTEM_DARK : 0),
		4,
	);
	Buffer.from(stored.frameId, 'hex').copy(header, BOOT_FRAME_ID_OFFSET);
	header.writeUInt16LE(stored.frame.viewportWidth, BOOT_FRAME_VIEWPORT_OFFSET);
	header.writeUInt16LE(stored.frame.viewportHeight, BOOT_FRAME_VIEWPORT_OFFSET + 2);
	return Buffer.concat([header, stored.frame.jpeg]);
}

function decodeFrameBody(body: Buffer): StoredBootFrame {
	if (body.byteLength <= BOOT_FRAME_HEADER_BYTES) {
		throw new BootStateCorruptError('frame is empty');
	}
	const pixelWidth = body.readUInt16LE(0);
	const pixelHeight = body.readUInt16LE(2);
	const flags = body.readUInt8(4);
	const viewportWidth = body.readUInt16LE(BOOT_FRAME_VIEWPORT_OFFSET);
	const viewportHeight = body.readUInt16LE(BOOT_FRAME_VIEWPORT_OFFSET + 2);
	if (pixelWidth === 0 || pixelHeight === 0 || viewportWidth === 0 || viewportHeight === 0) {
		throw new BootStateCorruptError('frame has no size');
	}
	if ((flags & ~BOOT_FRAME_KNOWN_FLAGS) !== 0) {
		throw new BootStateCorruptError('frame has unknown flags');
	}
	return {
		frameId: body.subarray(BOOT_FRAME_ID_OFFSET, BOOT_FRAME_VIEWPORT_OFFSET).toString('hex'),
		frame: {
			pixelWidth,
			pixelHeight,
			viewportWidth,
			viewportHeight,
			jpeg: body.subarray(BOOT_FRAME_HEADER_BYTES),
			systemDark: (flags & BOOT_FRAME_FLAG_FOLLOWS_SYSTEM) === 0 ? null : (flags & BOOT_FRAME_FLAG_SYSTEM_DARK) !== 0,
		},
		captureAppCheck: (flags & BOOT_FRAME_FLAG_CAPTURE_APP_CHECK) !== 0,
	};
}

function encodeStateBody(frameId: string | null, compressed: Buffer): Buffer {
	const frameIdBytes = frameId === null ? Buffer.alloc(0) : Buffer.from(frameId, 'hex');
	return Buffer.concat([Buffer.from([frameIdBytes.byteLength]), frameIdBytes, compressed]);
}

function decodeStateBody(body: Buffer): {readonly frameId: string | null; readonly compressed: Buffer} {
	const frameIdLength = body.byteLength === 0 ? -1 : body.readUInt8(0);
	if (frameIdLength !== 0 && frameIdLength !== BOOT_FRAME_ID_BYTES) {
		throw new BootStateCorruptError('bad frame id');
	}
	return {
		frameId: frameIdLength === 0 ? null : body.subarray(1, 1 + frameIdLength).toString('hex'),
		compressed: body.subarray(1 + frameIdLength),
	};
}

function detectNoCaptureApp(): Promise<boolean> {
	return getStreamerModeCaptureAppStatus().then(
		(status) => !status.detected,
		() => false,
	);
}

function withinBudget(check: Promise<boolean>, budgetMs: number): Promise<boolean> {
	return new Promise((resolve) => {
		const timer = setTimeout(() => resolve(false), budgetMs);
		void check.then((result) => {
			clearTimeout(timer);
			resolve(result);
		});
	});
}

class BootStateStore {
	private readonly directory: string;
	private readonly lastForegroundPath: string;
	private readonly lanes = new Map<string, AccountLane>();
	private lastForeground: string | null = null;
	private available = false;
	private prewarmStarted = false;
	private prewarm: Promise<void> = Promise.resolve();
	private prewarmDecryption: (() => void) | null = null;
	private held: HeldBootState | null = null;
	private unavailableWipe: Promise<void> | null = null;
	private wipeGeneration = 0;
	private nextFlushId = 0;
	private readonly flushes = new Map<number, PendingFlush>();

	public constructor(userDataPath: string) {
		this.directory = path.join(userDataPath, BOOT_STATE_DIRECTORY);
		this.lastForegroundPath = path.join(this.directory, LAST_FOREGROUND_FILE);
	}

	public admitServedFormat(declaredFormat: string | null): boolean {
		const held = this.held;
		if (!this.available || held === null) {
			return false;
		}
		if (declaredFormat === null || held.format !== declaredFormat) {
			log.info('The served renderer reads a different boot state format, booting without the stale view', {
				written: held.format,
				served: declaredFormat,
			});
			held.frame = null;
			held.admitted = false;
			return false;
		}
		held.admitted = true;
		return true;
	}

	public async prepareReload(): Promise<void> {
		if (!this.available) {
			return;
		}
		this.releaseHeld();
		const accountId = this.lastForeground;
		this.prewarm = this.loadCandidate(accountId).catch((error: unknown) => {
			log.warn('Boot state reload preparation failed', {error});
		});
		await this.prewarm;
		const captureAppFree = this.held?.captureAppFree ?? null;
		if (captureAppFree !== null) {
			await withinBudget(captureAppFree, CAPTURE_APP_CHECK_BUDGET_MS);
		}
	}

	public async whenPrewarmSettled(budgetMs: number | null): Promise<void> {
		if (budgetMs === null) {
			await this.prewarm;
			return;
		}
		await withinBudget(
			this.prewarm.then(() => true),
			budgetMs,
		);
	}

	public startPrewarm(): void {
		if (this.prewarmStarted) {
			return;
		}
		this.prewarmStarted = true;
		const captureAppFree = detectNoCaptureApp();
		const wipeGeneration = this.wipeGeneration;
		this.prewarm = new Promise((resolve) => {
			void this.readPrewarmFiles().then(
				(files) => {
					this.prewarmDecryption = () => {
						if (hasQuitStarted() || this.wipeGeneration !== wipeGeneration) {
							resolve();
							return;
						}
						this.available = isEncryptionUsable();
						if (!this.available) {
							log.info('Boot state is unavailable because safeStorage cannot encrypt');
							void this.wipeBecauseUnavailable();
							resolve();
							return;
						}
						this.lastForeground = files?.accountId ?? null;
						void this.loadCandidate(this.lastForeground, files, captureAppFree)
							.catch((error: unknown) => log.warn('Boot state prewarm failed', {error}))
							.finally(resolve);
					};
					this.decryptPrewarm();
				},
				(error: unknown) => {
					log.warn('Boot state prewarm failed', {error});
					resolve();
				},
			);
		});
		void this.prewarm
			.then(() => {
				if (this.available && !hasQuitStarted()) return this.pruneFiles(this.lastForeground);
				return undefined;
			})
			.catch((error: unknown) => {
				log.warn('Boot state pruning failed', {error});
			});
	}

	public decryptPrewarm(): void {
		if (!app.isReady()) return;
		const decrypt = this.prewarmDecryption;
		this.prewarmDecryption = null;
		decrypt?.();
	}

	public async read(accountKey: string): Promise<DesktopBootStateReadResult> {
		if (!this.ensureAvailable()) {
			return {status: 'unavailable'};
		}
		await this.prewarm;
		const accountId = bootStateAccountId(accountKey);
		const lane = this.lane(accountId);
		const generation = lane.generation;
		const loaded = this.takeHeld(accountId) ?? (await this.exclusive(lane, () => this.loadQuietly(accountId, lane)));
		if (lane.generation !== generation || loaded === null) {
			return {status: 'miss'};
		}
		return {status: 'hit', payload: loaded.payload, writtenAt: loaded.writtenAt};
	}

	public issueWriteToken(accountKey: string): string | null {
		if (!this.ensureAvailable()) {
			return null;
		}
		return this.issueToken(this.lane(bootStateAccountId(accountKey)));
	}

	public write(request: DesktopBootStateWriteRequest): Promise<DesktopBootStateWriteResult> {
		const accountId = bootStateAccountId(request.accountKey);
		const lane = this.lanes.get(accountId);
		if (lane === undefined || !lane.tokens.has(request.writeToken)) {
			return Promise.resolve('revoked');
		}
		if (request.payload.byteLength > BOOT_STATE_MAX_PAYLOAD_BYTES) {
			return Promise.resolve('too-large');
		}
		if (!this.ensureAvailable()) {
			return Promise.resolve('unavailable');
		}
		return new Promise((resolve) => {
			if (lane.pending !== null) {
				lane.pending.payload = request.payload;
				lane.pending.frameId = request.frameId;
				lane.pending.generation = lane.generation;
				lane.pending.waiters.push(resolve);
				return;
			}
			lane.pending = {
				payload: request.payload,
				frameId: request.frameId,
				generation: lane.generation,
				waiters: [resolve],
			};
			lane.tail = lane.tail.then(() => this.drain(accountId, lane));
		});
	}

	public clear(accountKey: string): Promise<void> {
		return this.clearAccountId(bootStateAccountId(accountKey));
	}

	public async retainAccounts(accountKeys: Iterable<string>): Promise<void> {
		const keep = new Set<string>();
		for (const accountKey of accountKeys) {
			keep.add(bootStateAccountId(accountKey));
		}
		const ids = new Set<string>(this.lanes.keys());
		for (const accountId of await this.listAccountFiles()) {
			ids.add(accountId);
		}
		await Promise.all(
			[...ids].filter((accountId) => !keep.has(accountId)).map((accountId) => this.clearAccountId(accountId)),
		);
	}

	public async wipeAll(): Promise<void> {
		this.wipeGeneration += 1;
		this.releaseHeld();
		this.lastForeground = null;
		const tails: Array<Promise<void>> = [];
		for (const lane of this.lanes.values()) {
			this.revokeLane(lane);
			tails.push(lane.tail);
		}
		await Promise.all(tails);
		await fsp.rm(this.directory, {recursive: true, force: true});
	}

	public requestFlush(
		target: Electron.WebContents | null,
		timeoutMs: number,
		options: DesktopBootStateFlushOptions,
	): Promise<BootStateFlushResult> {
		if (target === null || target.isDestroyed()) {
			return Promise.resolve('unavailable');
		}
		this.nextFlushId += 1;
		const id = this.nextFlushId;
		const startedAt = Date.now();
		return new Promise((resolve) => {
			const onDestroyed = (): void => settle('unavailable');
			const timer = setTimeout(() => settle('timed-out'), timeoutMs);
			const settle = (result: BootStateFlushResult): void => {
				if (!this.flushes.delete(id)) {
					return;
				}
				clearTimeout(timer);
				target.removeListener('destroyed', onDestroyed);
				log.info('Boot state flush settled', {result, frame: options.frame, elapsedMs: Date.now() - startedAt});
				resolve(result);
			};
			this.flushes.set(id, {target, settle});
			target.once('destroyed', onDestroyed);
			try {
				target.send(DESKTOP_BOOT_STATE_CHANNELS.flushRequested, id, options);
			} catch (error) {
				log.warn('Boot state flush request could not be sent', {error});
				settle('unavailable');
			}
		});
	}

	public acknowledgeFlush(sender: Electron.WebContents, id: number): void {
		const flush = this.flushes.get(id);
		if (flush !== undefined && flush.target === sender) {
			flush.settle('flushed');
		}
	}

	private ensureAvailable(): boolean {
		if (this.available) {
			return true;
		}
		this.available = isEncryptionUsable();
		if (!this.available) {
			void this.wipeBecauseUnavailable();
		}
		return this.available;
	}

	private wipeBecauseUnavailable(): Promise<void> {
		this.unavailableWipe ??= this.wipeAll().catch((error: unknown) => {
			log.warn('Failed to delete boot state while encryption is unavailable', {error});
		});
		return this.unavailableWipe;
	}

	private async readLastForeground(): Promise<string | null> {
		try {
			const accountId = (await fsp.readFile(this.lastForegroundPath, 'ascii')).trim();
			if (!ACCOUNT_ID_PATTERN.test(accountId)) {
				return null;
			}
			const stat = await fsp.stat(this.accountFilePath(accountId));
			if (!stat.isFile() || Date.now() - stat.mtimeMs > STALE_FILE_AGE_MS) {
				return null;
			}
			return accountId;
		} catch {
			return null;
		}
	}

	private async readPrewarmFiles(): Promise<PrewarmedBootFiles | null> {
		const names = fsp.readdir(this.directory).catch(() => []);
		const accountId = await this.readLastForeground();
		if (accountId === null) return null;
		const lane = this.lane(accountId);
		const generation = lane.generation;
		return this.exclusive(lane, async () => {
			const frameIds = (await names).flatMap((name) => {
				const match = BOOT_FRAME_FILE_PATTERN.exec(name);
				return match?.[1] === accountId ? [match[2]] : [];
			});
			const [state, frames] = await Promise.all([
				this.readStateFile(accountId),
				Promise.all(frameIds.map(async (frameId) => [frameId, await this.readFrameFile(accountId, frameId)] as const)),
			]);
			return {accountId, generation, state, frames: new Map(frames)};
		});
	}

	private async loadCandidate(
		accountId: string | null,
		files: PrewarmedBootFiles | null = null,
		captureAppFree: Promise<boolean> = detectNoCaptureApp(),
	): Promise<void> {
		if (accountId === null) {
			return;
		}
		const wipeGeneration = this.wipeGeneration;
		const lane = this.lane(accountId);
		const generation = lane.generation;
		if (files !== null && files.generation !== generation) return;
		const read = async () => {
			let frameRead: Promise<StoredBootFrame | null> = Promise.resolve(null);
			const loaded = await this.loadQuietly(
				accountId,
				lane,
				(frameId) => {
					if (frameId !== null) {
						frameRead = this.loadFrameQuietly(
							accountId,
							lane,
							frameId,
							files?.frames.get(frameId) ?? (files === null ? undefined : null),
						);
					}
				},
				files?.state,
			);
			return {loaded, frame: await frameRead};
		};
		const reading = files === null ? this.exclusive(lane, read) : read();
		if (files !== null) lane.tail = Promise.allSettled([lane.tail, reading]).then(() => undefined);
		const {loaded, frame} = await reading;
		const frameId = loaded?.frameId ?? null;
		if (loaded !== null && frameId === null) {
			log.info('The boot state was written without a boot frame');
		}
		if (loaded === null) {
			if (this.lastForeground === accountId) {
				this.lastForeground = null;
			}
			return;
		}
		if (lane.generation !== generation || this.wipeGeneration !== wipeGeneration) {
			return;
		}
		const timer = setTimeout(() => this.releaseHeld(), PREWARM_HOLD_MS);
		timer.unref();
		this.held = {
			accountId,
			payload: loaded.payload,
			writtenAt: loaded.writtenAt,
			frameId: loaded.frameId,
			format: readBootStatePayloadFormat(loaded.payload),
			timer,
			captureAppFree: frame?.captureAppCheck === true ? captureAppFree : null,
			frame,
			admitted: false,
		};
	}

	private takeHeld(accountId: string): LoadedBootState | null {
		const held = this.held;
		if (held === null || held.accountId !== accountId) {
			return null;
		}
		this.releaseHeld();
		return {payload: held.payload, writtenAt: held.writtenAt, frameId: held.frameId};
	}

	private releaseHeld(): void {
		if (this.held !== null) {
			clearTimeout(this.held.timer);
			this.held = null;
		}
	}

	private lane(accountId: string): AccountLane {
		let lane = this.lanes.get(accountId);
		if (lane === undefined) {
			lane = {
				generation: 0,
				tokens: new Set(),
				dataKey: null,
				capturedFrameId: null,
				pending: null,
				tail: Promise.resolve(),
			};
			this.lanes.set(accountId, lane);
		}
		return lane;
	}

	private exclusive<T>(lane: AccountLane, operation: () => Promise<T>): Promise<T> {
		const result = lane.tail.then(operation);
		lane.tail = result.then(
			() => undefined,
			() => undefined,
		);
		return result;
	}

	private issueToken(lane: AccountLane): string {
		const token = randomBytes(16).toString('hex');
		lane.tokens.add(token);
		if (lane.tokens.size > MAX_LIVE_TOKENS_PER_ACCOUNT) {
			const oldest = lane.tokens.values().next().value;
			if (oldest !== undefined) {
				lane.tokens.delete(oldest);
			}
		}
		return token;
	}

	private revokeLane(lane: AccountLane): void {
		lane.generation += 1;
		lane.tokens.clear();
		lane.dataKey = null;
		lane.capturedFrameId = null;
		const pending = lane.pending;
		lane.pending = null;
		if (pending !== null) {
			for (const waiter of pending.waiters) {
				waiter('revoked');
			}
		}
	}

	private clearAccountId(accountId: string): Promise<void> {
		const lane = this.lane(accountId);
		this.revokeLane(lane);
		if (this.held?.accountId === accountId) {
			this.releaseHeld();
		}
		if (this.lastForeground === accountId) {
			this.lastForeground = null;
		}
		lane.tail = lane.tail.then(() => this.deleteAccountFiles(accountId));
		return lane.tail;
	}

	private async deleteAccountFiles(accountId: string): Promise<void> {
		try {
			await fsp.rm(this.accountFilePath(accountId), {force: true});
			await this.removeFrames(accountId, new Set());
			const lastForeground = await fsp.readFile(this.lastForegroundPath, 'ascii').catch(() => null);
			if (lastForeground?.trim() === accountId) {
				await fsp.rm(this.lastForegroundPath, {force: true});
			}
		} catch (error) {
			log.warn('Failed to delete a boot state file', {error});
		}
	}

	private async drain(accountId: string, lane: AccountLane): Promise<void> {
		const pending = lane.pending;
		if (pending === null) {
			return;
		}
		lane.pending = null;
		let result: DesktopBootStateWriteResult;
		try {
			result = await this.persist(accountId, lane, pending);
		} catch (error) {
			log.warn('Failed to write the boot state', {error});
			result = 'failed';
		}
		for (const waiter of pending.waiters) {
			waiter(result);
		}
	}

	private async persist(
		accountId: string,
		lane: AccountLane,
		pending: PendingWrite,
	): Promise<DesktopBootStateWriteResult> {
		if (pending.generation !== lane.generation) {
			return 'revoked';
		}
		const compressed = await compressAsync(pending.payload, {
			params: {[zlibConstants.ZSTD_c_compressionLevel]: ZSTD_LEVEL},
		});
		if (pending.generation !== lane.generation) {
			return 'revoked';
		}
		const dataKey = lane.dataKey ?? createDataKey();
		const container = encodeContainer(
			accountId,
			ALGORITHM_ZSTD_AES_256_GCM,
			dataKey,
			encodeStateBody(pending.frameId, compressed),
			Date.now(),
		);
		await fsp.mkdir(this.directory, {recursive: true, mode: DIRECTORY_MODE});
		if (pending.generation !== lane.generation) {
			return 'revoked';
		}
		await writeFileAtomically(this.accountFilePath(accountId), container, FILE_MODE);
		await this.removeFrames(
			accountId,
			new Set([pending.frameId, lane.capturedFrameId].filter((frameId) => frameId !== null)),
		);
		if (pending.generation !== lane.generation) {
			return 'revoked';
		}
		lane.dataKey = dataKey;
		if (this.lastForeground !== accountId) {
			await writeFileAtomically(this.lastForegroundPath, accountId, FILE_MODE);
			if (pending.generation === lane.generation) {
				this.lastForeground = accountId;
			}
		}
		return 'written';
	}

	private async readStateFile(accountId: string): Promise<Buffer | null> {
		const filePath = this.accountFilePath(accountId);
		try {
			const stat = await fsp.stat(filePath);
			if (stat.size > MAX_FILE_BYTES) {
				throw new BootStateCorruptError('file too large');
			}
			const file = await fsp.readFile(filePath);
			if (file.byteLength > MAX_FILE_BYTES) throw new BootStateCorruptError('file too large');
			return file;
		} catch (error) {
			if (error instanceof BootStateCorruptError) {
				await this.discard(accountId, error);
			}
			return null;
		}
	}

	private async loadQuietly(
		accountId: string,
		lane: AccountLane,
		onFrameId?: (frameId: string | null) => void,
		prefetchedFile?: Buffer | null,
	): Promise<LoadedBootState | null> {
		const generation = lane.generation;
		const file = prefetchedFile === undefined ? await this.readStateFile(accountId) : prefetchedFile;
		if (file === null) return null;
		try {
			const container = decodeContainer(file, ALGORITHM_ZSTD_AES_256_GCM);
			const key = this.containerKey(lane, container);
			const {frameId, compressed} = decodeStateBody(
				decryptContainer(accountId, ALGORITHM_ZSTD_AES_256_GCM, container, key),
			);
			if (lane.generation === generation) {
				lane.dataKey ??= {key, wrapped: Buffer.from(container.wrapped)};
			}
			onFrameId?.(frameId);
			const payload = await decompressAsync(compressed, {maxOutputLength: BOOT_STATE_MAX_PAYLOAD_BYTES});
			return {payload, writtenAt: container.writtenAt, frameId};
		} catch (error) {
			await this.discard(accountId, error);
			return null;
		}
	}

	private containerKey(lane: AccountLane, container: DecodedContainer): Buffer {
		const cached = lane.dataKey;
		return cached?.wrapped.equals(container.wrapped) ? cached.key : unwrapDataKey(container.wrapped);
	}

	private async readFrameFile(accountId: string, frameId: string): Promise<Buffer | null> {
		const filePath = this.frameFilePath(accountId, frameId);
		try {
			const stat = await fsp.stat(filePath);
			if (stat.size > MAX_BOOT_FRAME_FILE_BYTES) {
				await fsp.rm(filePath, {force: true});
				return null;
			}
			const file = await fsp.readFile(filePath);
			if (file.byteLength > MAX_BOOT_FRAME_FILE_BYTES) {
				await fsp.rm(filePath, {force: true});
				return null;
			}
			return file;
		} catch {
			log.info('No boot frame is on disk for the boot state');
			return null;
		}
	}

	private async loadFrameQuietly(
		accountId: string,
		lane: AccountLane,
		pairedFrameId: string,
		prefetchedFile?: Buffer | null,
	): Promise<StoredBootFrame | null> {
		const filePath = this.frameFilePath(accountId, pairedFrameId);
		const file = prefetchedFile === undefined ? await this.readFrameFile(accountId, pairedFrameId) : prefetchedFile;
		if (file === null) return null;
		try {
			const container = decodeContainer(file, ALGORITHM_BOOT_FRAME_AES_256_GCM);
			const key = this.containerKey(lane, container);
			const stored = decodeFrameBody(
				decryptContainer(`${accountId}${BOOT_FRAME_AAD_SUFFIX}`, ALGORITHM_BOOT_FRAME_AES_256_GCM, container, key),
			);
			if (stored.frameId !== pairedFrameId) {
				log.info('Discarding a boot frame that does not belong to the boot state');
				await fsp.rm(filePath, {force: true}).catch(() => undefined);
				return null;
			}
			return stored;
		} catch (error) {
			log.warn('Discarding an unreadable boot frame', {
				reason: error instanceof Error ? error.message : String(error),
			});
			await fsp.rm(filePath, {force: true}).catch(() => undefined);
			return null;
		}
	}

	public captureFrame(
		sender: Electron.WebContents,
		request: DesktopBootStateFrameRequest,
	): Promise<DesktopBootStateFrameResult> {
		const accountId = bootStateAccountId(request.accountKey);
		const lane = this.lanes.get(accountId);
		if (lane === undefined || !lane.tokens.has(request.writeToken)) {
			return Promise.resolve({status: 'revoked'});
		}
		if (!this.ensureAvailable()) {
			return Promise.resolve({status: 'unavailable'});
		}
		const generation = lane.generation;
		return this.exclusive(lane, () => this.persistFrame(sender, accountId, lane, generation, request)).catch(
			(error: unknown) => {
				log.warn('Failed to capture the boot frame', {error});
				return {status: 'failed'} as const;
			},
		);
	}

	private async persistFrame(
		sender: Electron.WebContents,
		accountId: string,
		lane: AccountLane,
		generation: number,
		{captureAppCheck, systemDark}: DesktopBootStateFrameRequest,
	): Promise<DesktopBootStateFrameResult> {
		const window = BrowserWindow.fromWebContents(sender);
		if (window === null || window.isDestroyed() || !window.isVisible() || window.isMinimized()) {
			log.info('Skipped the boot frame because the window is not on screen');
			return {status: 'skipped'};
		}
		const [viewportWidth = 0, viewportHeight = 0] = window.getContentSize();
		const image = await sender.capturePage();
		const {width, height} = image.getSize();
		if (
			image.isEmpty() ||
			width === 0 ||
			height === 0 ||
			width > 0xffff ||
			height > 0xffff ||
			viewportWidth <= 0 ||
			viewportHeight <= 0 ||
			viewportWidth > 0xffff ||
			viewportHeight > 0xffff
		) {
			log.info('Skipped the boot frame because the captured page is empty');
			return {status: 'skipped'};
		}
		const jpeg = image.toJPEG(BOOT_FRAME_JPEG_QUALITY);
		if (lane.generation !== generation) {
			return {status: 'revoked'};
		}
		const frameId = randomBytes(BOOT_FRAME_ID_BYTES).toString('hex');
		const dataKey = lane.dataKey ?? createDataKey();
		const container = encodeContainer(
			`${accountId}${BOOT_FRAME_AAD_SUFFIX}`,
			ALGORITHM_BOOT_FRAME_AES_256_GCM,
			dataKey,
			encodeFrameBody({
				frameId,
				frame: {jpeg, pixelWidth: width, pixelHeight: height, viewportWidth, viewportHeight, systemDark},
				captureAppCheck,
			}),
			Date.now(),
		);
		await fsp.mkdir(this.directory, {recursive: true, mode: DIRECTORY_MODE});
		await writeFileAtomically(this.frameFilePath(accountId, frameId), container, FILE_MODE);
		if (lane.generation !== generation) {
			await fsp.rm(this.frameFilePath(accountId, frameId), {force: true});
			return {status: 'revoked'};
		}
		lane.dataKey = dataKey;
		lane.capturedFrameId = frameId;
		log.info('Captured the boot frame', {width, height, viewportWidth, viewportHeight, captureAppCheck});
		return {status: 'captured', frameId};
	}

	public async takeFrame(activeAccountKey: Promise<string | null>): Promise<DesktopBootFrame | null> {
		const [accountKey] = await Promise.all([activeAccountKey, this.prewarm]);
		const held = this.held;
		const stored = held?.frame ?? null;
		if (held === null || stored === null || !held.admitted) {
			return null;
		}
		held.frame = null;
		if (accountKey === null || held.accountId !== bootStateAccountId(accountKey)) {
			log.info('Withheld the boot frame because another account is booting');
			this.releaseHeld();
			return null;
		}
		const generation = this.lane(held.accountId).generation;
		if (held.captureAppFree !== null && !(await withinBudget(held.captureAppFree, CAPTURE_APP_CHECK_BUDGET_MS))) {
			log.info('Withheld the boot frame because a capture app may be running');
			return null;
		}
		if (this.lane(held.accountId).generation !== generation) return null;
		return stored.frame;
	}

	private async discard(accountId: string, error: unknown): Promise<void> {
		log.warn('Discarding an unreadable boot state file', {
			reason: error instanceof Error ? error.message : String(error),
		});
		const lane = this.lane(accountId);
		lane.dataKey = null;
		await fsp.rm(this.accountFilePath(accountId), {force: true}).catch(() => undefined);
		await this.removeFrames(accountId, new Set()).catch(() => undefined);
	}

	private async removeFrames(accountId: string, keep: ReadonlySet<string>): Promise<void> {
		let names: Array<string>;
		try {
			names = await fsp.readdir(this.directory);
		} catch {
			return;
		}
		for (const name of names) {
			const match = BOOT_FRAME_FILE_PATTERN.exec(name);
			if (match?.[1] === accountId && !keep.has(match[2] ?? '')) {
				await fsp.rm(path.join(this.directory, name), {force: true});
			}
		}
	}

	private async listAccountFiles(): Promise<Array<string>> {
		let names: Array<string>;
		try {
			names = await fsp.readdir(this.directory);
		} catch {
			return [];
		}
		return names
			.filter((name) => name.endsWith(BOOT_STATE_FILE_EXTENSION))
			.map((name) => name.slice(0, -BOOT_STATE_FILE_EXTENSION.length))
			.filter((accountId) => ACCOUNT_ID_PATTERN.test(accountId));
	}

	private async pruneFiles(keepAccountId: string | null): Promise<void> {
		let names: Array<string>;
		try {
			names = await fsp.readdir(this.directory);
		} catch {
			return;
		}
		const now = Date.now();
		const kept: Array<{accountId: string; mtimeMs: number}> = [];
		for (const name of names) {
			const filePath = path.join(this.directory, name);
			const stat = await fsp.stat(filePath).catch(() => null);
			if (stat === null) {
				continue;
			}
			if (name.endsWith(TEMPORARY_FILE_SUFFIX)) {
				if (now - stat.mtimeMs > STALE_TEMPORARY_FILE_AGE_MS) {
					await fsp.rm(filePath, {force: true}).catch(() => undefined);
				}
				continue;
			}
			if (name.endsWith(BOOT_FRAME_FILE_EXTENSION)) {
				const frameAccountId = BOOT_FRAME_FILE_PATTERN.exec(name)?.[1];
				if (frameAccountId === undefined || !names.includes(`${frameAccountId}${BOOT_STATE_FILE_EXTENSION}`)) {
					await fsp.rm(filePath, {force: true}).catch(() => undefined);
				}
				continue;
			}
			if (!name.endsWith(BOOT_STATE_FILE_EXTENSION)) {
				continue;
			}
			const accountId = name.slice(0, -BOOT_STATE_FILE_EXTENSION.length);
			if (!ACCOUNT_ID_PATTERN.test(accountId)) {
				await fsp.rm(filePath, {force: true}).catch(() => undefined);
				continue;
			}
			if (now - stat.mtimeMs > STALE_FILE_AGE_MS) {
				await this.clearAccountId(accountId);
				continue;
			}
			kept.push({accountId, mtimeMs: stat.mtimeMs});
		}
		const rank = (entry: {accountId: string; mtimeMs: number}): number =>
			entry.accountId === keepAccountId ? Number.POSITIVE_INFINITY : entry.mtimeMs;
		kept.sort((left, right) => rank(right) - rank(left));
		for (const entry of kept.slice(MAX_ACCOUNT_FILES)) {
			await this.clearAccountId(entry.accountId);
		}
	}

	private accountFilePath(accountId: string): string {
		return path.join(this.directory, `${accountId}${BOOT_STATE_FILE_EXTENSION}`);
	}

	private frameFilePath(accountId: string, frameId: string): string {
		return path.join(this.directory, `${accountId}.${frameId}${BOOT_FRAME_FILE_EXTENSION}`);
	}
}

let store: BootStateStore | null = null;
export function takeBootFrame(
	userDataPath: string,
	activeAccountKey: Promise<string | null>,
): Promise<DesktopBootFrame | null> {
	return storeFor(userDataPath).takeFrame(activeAccountKey);
}

function storeFor(userDataPath: string): BootStateStore {
	store ??= new BootStateStore(userDataPath);
	return store;
}

export function prewarmBootState(userDataPath: string): void {
	try {
		storeFor(userDataPath).startPrewarm();
	} catch (error) {
		log.warn('Boot state prewarm could not start', {error});
	}
}

export function decryptBootStatePrewarm(): void {
	store?.decryptPrewarm();
}

export function registerBootStateIpc(
	userDataPath: string,
	frameRoutes: {
		readonly takeFrame: (sender: Electron.WebContents) => Promise<DesktopBootFrame | null>;
		readonly isAppDocumentWindowContents: (sender: Electron.WebContents) => boolean;
		readonly isPrivilegedRendererDocumentSender: (
			event: Electron.IpcMainInvokeEvent | Electron.IpcMainEvent,
		) => boolean;
	},
): void {
	const bootState = storeFor(userDataPath);
	registerBootStateIpcRoutes({
		read: (accountKey) => bootState.read(accountKey),
		issueWriteToken: async (accountKey) => bootState.issueWriteToken(accountKey),
		write: (request) => bootState.write(request),
		clear: (accountKey) => bootState.clear(accountKey),
		flushed: (sender, id) => bootState.acknowledgeFlush(sender, id),
		captureFrame: (sender, request) => bootState.captureFrame(sender, request),
		takeFrame: frameRoutes.takeFrame,
		isAppDocumentWindowContents: frameRoutes.isAppDocumentWindowContents,
		isPrivilegedRendererDocumentSender: frameRoutes.isPrivilegedRendererDocumentSender,
	});
}

export function requestBootStateFlush(
	target: Electron.WebContents | null,
	timeoutMs: number,
	options: DesktopBootStateFlushOptions = {frame: true},
): Promise<BootStateFlushResult> {
	if (store === null) {
		return Promise.resolve('unavailable');
	}
	return store.requestFlush(target, timeoutMs, options);
}

export async function wipeAllBootState(userDataPath: string): Promise<void> {
	try {
		await storeFor(userDataPath).wipeAll();
	} catch (error) {
		log.warn('Failed to wipe boot state', {error});
	}
}

export async function retainBootStateForAccounts(accountKeys: Iterable<string>): Promise<void> {
	if (store === null) {
		return;
	}
	try {
		await store.retainAccounts(accountKeys);
	} catch (error) {
		log.warn('Failed to prune boot state for removed accounts', {error});
	}
}

export function whenBootStatePrewarmed(budgetMs: number | null = INDEX_PREWARM_WAIT_BUDGET_MS): Promise<void> {
	return store?.whenPrewarmSettled(budgetMs) ?? Promise.resolve();
}

export function admitBootStateFormat(declaredFormat: string | null): boolean {
	return store?.admitServedFormat(declaredFormat) ?? false;
}

export async function prepareBootStateReload(): Promise<void> {
	if (store === null) {
		return;
	}
	await store.prepareReload();
}
