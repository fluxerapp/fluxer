// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RetainedCodeHighlight} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {SNAPSHOT_SCHEMA_EPOCH, type SnapshotAccountMetadataRow} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSnapshotEntries, StateSyncCursor} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {RetainedMarkdownParse} from '@app/features/messaging/utils/markdown/MarkdownParseCache';
import type {ProfileWire} from '@app/features/user/models/Profile';
import {BOOT_STATE_MAX_PAYLOAD_BYTES, BOOT_STATE_PAYLOAD_VERSION} from '@fluxer/desktop_ipc/src/BootStateContract';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

export const BOOT_STATE_MAX_MESSAGE_CHANNELS = 25;
export const BOOT_STATE_MAX_MESSAGES_PER_CHANNEL = 100;
export const BOOT_STATE_MAX_MEMBER_LISTS = 4;
export const BOOT_STATE_MAX_MEMBER_LIST_ROWS = 300;

const MAGIC = [0x46, 0x58, 0x42, 0x50] as const;
const HEADER_BYTES = 10;
const TABLE_ENTRY_BYTES = 9;
const MAX_VARINT_BYTES = 5;
const ACCOUNT_METADATA_ENTITY = 'account_metadata';

const SectionKind = {
	META: 1,
	SHELL: 2,
	MENTIONS: 3,
	MESSAGES: 4,
	MEMBERS: 5,
	VIEWPORT: 6,
	GUILD_READ_STATE: 7,
} as const;

const MessageWindowFlag = {
	HAS_MORE_BEFORE: 1,
	HAS_MORE_AFTER: 2,
} as const;

export interface BootStateFrameRecord {
	readonly id: string;
	readonly signature: string;
	readonly layout: string | null;
}

interface BootStateMeta {
	readonly frame: BootStateFrameRecord | null;
	readonly accountKey: string;
	readonly userId: string;
}

interface BootStateShell {
	readonly entries: StateSnapshotEntries;
	readonly cursor: StateSyncCursor;
}

export interface BootStateSerializableMessage {
	toBootStateJSON(): unknown;
}

type BootStateEncodableMessage = string | BootStateSerializableMessage;

export interface BootStateMessageWindow<M = string> {
	readonly channelId: string;
	readonly hasMoreBefore: boolean;
	readonly hasMoreAfter: boolean;
	readonly messages: ReadonlyArray<M>;
}

interface BootStateMemberGroup {
	readonly id: string;
	readonly count: number;
}

export interface BootStateMemberRow {
	readonly type: 'group' | 'member';
	readonly group?: BootStateMemberGroup;
	readonly userId?: string;
	readonly member?: GuildMemberData;
	readonly presence?: {
		readonly status?: string;
		readonly mobile?: boolean;
		readonly custom_status?: unknown;
	} | null;
}

export interface BootStateMemberList {
	readonly guildId: string;
	readonly channelId: string;
	readonly listId: string;
	readonly groups: ReadonlyArray<BootStateMemberGroup>;
	readonly rows: ReadonlyArray<readonly [number, BootStateMemberRow]>;
	readonly memberCount: number;
	readonly onlineCount: number;
}

export interface BootStateUnreadBoundary {
	readonly ackMessageId: string | null;
	readonly oldestUnreadMessageId: string | null;
	readonly visualUnreadMessageId: string | null;
}

export interface BootStateUnreadView {
	readonly ackMessageId: string | null;
	readonly lastMessageId: string;
	readonly unreadCount: number;
	readonly estimated: boolean;
	readonly oldestUnreadMessageId: string | null;
}

export interface BootStateViewportChannel {
	readonly unreadBoundary?: BootStateUnreadBoundary;
	readonly unread: BootStateUnreadView | null;
	readonly atBottom: boolean;
	readonly anchorMessageId: string | null;
	readonly anchorOffsetPx: number;
	readonly scrollTop: number | null;
	readonly scrollHeight: number | null;
	readonly offsetHeight: number | null;
	readonly messageTopsPx?: Readonly<Record<string, number>>;
	readonly tailHeightPx?: number;
}

interface BootStateViewport {
	readonly dmProfiles?: ReadonlyArray<ProfileWire>;
	readonly channels: Readonly<Record<string, BootStateViewportChannel>>;
	readonly guildListScrollTop: number;
	readonly imageSources: ReadonlyArray<string>;
	readonly codeHighlights: ReadonlyArray<RetainedCodeHighlight>;
	readonly markdownParses: ReadonlyArray<RetainedMarkdownParse>;
	readonly rendererBuild: string;
	readonly userAreaOverlayHeightPx: number | null;
}

export interface BootStateGuildReadState {
	readonly unread: boolean;
	readonly mentionCount: number;
	readonly firstUnreadChannelId: string | null;
}

export interface BootStatePayload<M = string> {
	readonly meta: BootStateMeta;
	readonly shell: BootStateShell;
	readonly mentions: Readonly<Record<string, number>>;
	readonly guildReadState: Readonly<Record<string, BootStateGuildReadState>>;
	readonly messages: ReadonlyArray<BootStateMessageWindow<M>>;
	readonly members: ReadonlyArray<BootStateMemberList>;
	readonly viewport: BootStateViewport;
}

interface BootStateExpectedAccount {
	readonly accountKey: string;
	readonly userId: string;
}

interface BootStateEncodeOptions {
	readonly maxBytes?: number;
	readonly forbiddenMetaValues?: ReadonlyArray<string>;
}

class BootStateDecodeError extends Error {
	constructor(readonly reason: string) {
		super(`Boot state payload rejected: ${reason}`);
		this.name = 'BootStateDecodeError';
	}
}

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder('utf-8', {fatal: true});

function varintSize(value: number): number {
	let size = 1;
	let remaining = value >>> 7;
	while (remaining > 0) {
		size += 1;
		remaining >>>= 7;
	}
	return size;
}

class ByteSink {
	readonly chunks: Array<Uint8Array> = [];
	byteLength = 0;

	varint(value: number): void {
		if (!Number.isInteger(value) || value < 0 || value > 0xffffffff) {
			throw new Error(`Boot state varint out of range: ${value}`);
		}
		const bytes = new Uint8Array(varintSize(value));
		let remaining = value;
		for (let index = 0; index < bytes.length; index += 1) {
			const low = remaining & 0x7f;
			remaining = Math.floor(remaining / 128);
			bytes[index] = remaining > 0 ? low | 0x80 : low;
		}
		this.push(bytes);
	}

	u8(value: number): void {
		this.push(Uint8Array.of(value & 0xff));
	}

	string(value: string): void {
		const bytes = textEncoder.encode(value);
		this.varint(bytes.byteLength);
		this.push(bytes);
	}

	append(other: ByteSink): void {
		for (const chunk of other.chunks) {
			this.push(chunk);
		}
	}

	private push(bytes: Uint8Array): void {
		this.chunks.push(bytes);
		this.byteLength += bytes.byteLength;
	}
}

function stripAccountMetadata(value: string): string {
	const row = JSON.parse(value) as SnapshotAccountMetadataRow;
	return JSON.stringify({...row, authSessionIdHash: null, webAuthnCredentials: []});
}

function encodeMeta(meta: BootStateMeta, forbiddenValues: ReadonlyArray<string>): ByteSink {
	const json = JSON.stringify(meta);
	if (/"token"\s*:/i.test(json)) {
		throw new Error('Boot state META must not contain a token');
	}
	for (const forbidden of forbiddenValues) {
		if (forbidden.length > 0 && json.includes(forbidden)) {
			throw new Error('Boot state META must not contain a secret');
		}
	}
	const sink = new ByteSink();
	sink.string(json);
	return sink;
}

function* encodeShell(shell: BootStateShell): Generator<void, ByteSink, void> {
	const sink = new ByteSink();
	const entities = Object.entries(shell.entries);
	sink.varint(entities.length);
	for (const [entity, rows] of entities) {
		const keys = Object.keys(rows);
		sink.string(entity);
		sink.varint(keys.length);
		for (const key of keys) {
			const value = rows[key]!;
			sink.string(key);
			sink.string(entity === ACCOUNT_METADATA_ENTITY ? stripAccountMetadata(value) : value);
			yield;
		}
	}
	sink.string(JSON.stringify({...shell.cursor, sessionId: null}));
	return sink;
}

function encodeJsonSection(value: unknown): ByteSink {
	const sink = new ByteSink();
	sink.string(JSON.stringify(value));
	return sink;
}

function* encodeMessageWindow(
	window: BootStateMessageWindow<BootStateEncodableMessage>,
): Generator<void, ByteSink, void> {
	const sink = new ByteSink();
	sink.string(window.channelId);
	sink.u8(
		(window.hasMoreBefore ? MessageWindowFlag.HAS_MORE_BEFORE : 0) |
			(window.hasMoreAfter ? MessageWindowFlag.HAS_MORE_AFTER : 0),
	);
	const messages = window.messages.slice(-BOOT_STATE_MAX_MESSAGES_PER_CHANNEL);
	sink.varint(messages.length);
	for (const message of messages) {
		sink.string(typeof message === 'string' ? message : JSON.stringify(message.toBootStateJSON()));
		yield;
	}
	return sink;
}

function utf8Length(value: string): number {
	return textEncoder.encode(value).byteLength;
}

function capMemberList(list: BootStateMemberList): BootStateMemberList {
	return list.rows.length <= BOOT_STATE_MAX_MEMBER_LIST_ROWS
		? list
		: {...list, rows: list.rows.slice(0, BOOT_STATE_MAX_MEMBER_LIST_ROWS)};
}

interface EncodedSection {
	readonly kind: number;
	readonly sink: ByteSink;
}

function assemble(sections: ReadonlyArray<EncodedSection>): Uint8Array {
	const tableBytes = sections.length * TABLE_ENTRY_BYTES;
	let total = HEADER_BYTES + tableBytes;
	for (const section of sections) {
		total += section.sink.byteLength;
	}
	const out = new Uint8Array(total);
	const view = new DataView(out.buffer);
	out.set(MAGIC, 0);
	view.setUint16(4, BOOT_STATE_PAYLOAD_VERSION, true);
	view.setUint16(6, SNAPSHOT_SCHEMA_EPOCH, true);
	view.setUint16(8, sections.length, true);
	let offset = HEADER_BYTES + tableBytes;
	sections.forEach((section, index) => {
		const entry = HEADER_BYTES + index * TABLE_ENTRY_BYTES;
		view.setUint8(entry, section.kind);
		view.setUint32(entry + 1, offset, true);
		view.setUint32(entry + 5, section.sink.byteLength, true);
		for (const chunk of section.sink.chunks) {
			out.set(chunk, offset);
			offset += chunk.byteLength;
		}
	});
	return out;
}

export type BootStateEncodablePayload = BootStatePayload<BootStateEncodableMessage>;

export function* encodeBootStatePayloadSteps(
	payload: BootStateEncodablePayload,
	options: BootStateEncodeOptions = {},
): Generator<void, Uint8Array | null, void> {
	const maxBytes = options.maxBytes ?? BOOT_STATE_MAX_PAYLOAD_BYTES;
	const meta = encodeMeta(payload.meta, options.forbiddenMetaValues ?? []);
	const shell = yield* encodeShell(payload.shell);
	const mentions = encodeJsonSection(payload.mentions);
	const guildReadState = encodeJsonSection(payload.guildReadState);
	const viewport = encodeJsonSection(payload.viewport);
	const windows: Array<ByteSink> = [];
	for (const window of payload.messages.slice(0, BOOT_STATE_MAX_MESSAGE_CHANNELS)) {
		windows.push(yield* encodeMessageWindow(window));
	}
	const memberJson = payload.members
		.slice(0, BOOT_STATE_MAX_MEMBER_LISTS)
		.map((list) => JSON.stringify(capMemberList(list)));
	yield;
	const memberBytes = memberJson.map(utf8Length);
	const fixedBytes =
		HEADER_BYTES +
		7 * TABLE_ENTRY_BYTES +
		meta.byteLength +
		shell.byteLength +
		mentions.byteLength +
		guildReadState.byteLength +
		viewport.byteLength;
	if (fixedBytes > maxBytes) {
		return null;
	}
	let windowCount = windows.length;
	let memberCount = memberJson.length;
	const windowBytes = () => {
		let sum = varintSize(windowCount);
		for (let index = 0; index < windowCount; index += 1) {
			sum += windows[index]!.byteLength;
		}
		return sum;
	};
	const membersBytes = () => {
		let sum = 2 + Math.max(0, memberCount - 1);
		for (let index = 0; index < memberCount; index += 1) {
			sum += memberBytes[index]!;
		}
		return sum + varintSize(sum);
	};
	while (windowCount > 0 && fixedBytes + windowBytes() + membersBytes() > maxBytes) {
		windowCount -= 1;
	}
	while (memberCount > 0 && fixedBytes + windowBytes() + membersBytes() > maxBytes) {
		memberCount -= 1;
	}
	if (fixedBytes + windowBytes() + membersBytes() > maxBytes) {
		return null;
	}
	const messages = new ByteSink();
	messages.varint(windowCount);
	for (let index = 0; index < windowCount; index += 1) {
		messages.append(windows[index]!);
	}
	const members = new ByteSink();
	members.string(`[${memberJson.slice(0, memberCount).join(',')}]`);
	yield;
	return assemble([
		{kind: SectionKind.META, sink: meta},
		{kind: SectionKind.SHELL, sink: shell},
		{kind: SectionKind.MENTIONS, sink: mentions},
		{kind: SectionKind.MESSAGES, sink: messages},
		{kind: SectionKind.MEMBERS, sink: members},
		{kind: SectionKind.VIEWPORT, sink: viewport},
		{kind: SectionKind.GUILD_READ_STATE, sink: guildReadState},
	]);
}

class ByteSource {
	private position: number;

	constructor(
		private readonly bytes: Uint8Array,
		start: number,
		private readonly end: number,
	) {
		this.position = start;
	}

	get done(): boolean {
		return this.position === this.end;
	}

	varint(): number {
		let value = 0;
		let scale = 1;
		for (let index = 0; index < MAX_VARINT_BYTES; index += 1) {
			if (this.position >= this.end) {
				throw new BootStateDecodeError('varint runs out of bounds');
			}
			const byte = this.bytes[this.position]!;
			this.position += 1;
			value += (byte & 0x7f) * scale;
			if ((byte & 0x80) === 0) {
				if (value > 0xffffffff) {
					throw new BootStateDecodeError('varint too large');
				}
				return value;
			}
			scale *= 128;
		}
		throw new BootStateDecodeError('varint too long');
	}

	u8(): number {
		if (this.position >= this.end) {
			throw new BootStateDecodeError('byte runs out of bounds');
		}
		const value = this.bytes[this.position]!;
		this.position += 1;
		return value;
	}

	string(): string {
		const length = this.varint();
		if (length > this.end - this.position) {
			throw new BootStateDecodeError('string runs out of bounds');
		}
		const start = this.position;
		this.position += length;
		try {
			return textDecoder.decode(this.bytes.subarray(start, this.position));
		} catch {
			throw new BootStateDecodeError('string is not valid UTF-8');
		}
	}

	count(): number {
		const value = this.varint();
		if (value > this.end - this.position) {
			throw new BootStateDecodeError('count exceeds section size');
		}
		return value;
	}

	finish(): void {
		if (!this.done) {
			throw new BootStateDecodeError('section has trailing bytes');
		}
	}
}

function parseJson(source: string, label: string): unknown {
	try {
		return JSON.parse(source);
	} catch {
		throw new BootStateDecodeError(`${label} is not JSON`);
	}
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isNullableString(value: unknown): value is string | null {
	return value === null || typeof value === 'string';
}

function decodeFrameRecord(value: unknown): BootStateFrameRecord | null {
	if (value === null) {
		return null;
	}
	if (
		!isRecord(value) ||
		typeof value.id !== 'string' ||
		typeof value.signature !== 'string' ||
		(value.layout !== undefined && value.layout !== null && typeof value.layout !== 'string')
	) {
		throw new BootStateDecodeError('META has a malformed frame');
	}
	return {id: value.id, signature: value.signature, layout: value.layout ?? null};
}

function decodeMeta(source: ByteSource, expected: BootStateExpectedAccount): BootStateMeta {
	const meta = parseJson(source.string(), 'META');
	source.finish();
	if (!isRecord(meta) || typeof meta.accountKey !== 'string' || typeof meta.userId !== 'string') {
		throw new BootStateDecodeError('META is malformed');
	}
	const frame = decodeFrameRecord(meta.frame);
	if (meta.accountKey !== expected.accountKey || meta.userId !== expected.userId) {
		throw new BootStateDecodeError('META belongs to another account');
	}
	return {frame, accountKey: meta.accountKey, userId: meta.userId};
}

function decodeShell(source: ByteSource): BootStateShell {
	const entries: StateSnapshotEntries = {};
	const entityCount = source.count();
	for (let entityIndex = 0; entityIndex < entityCount; entityIndex += 1) {
		const entity = source.string();
		if (entity.length === 0 || Object.hasOwn(entries, entity)) {
			throw new BootStateDecodeError('SHELL entity is invalid');
		}
		const rowCount = source.count();
		const rows: Record<string, string> = {};
		for (let rowIndex = 0; rowIndex < rowCount; rowIndex += 1) {
			const key = source.string();
			rows[key] = source.string();
		}
		entries[entity] = rows;
	}
	const cursor = parseJson(source.string(), 'SHELL cursor');
	source.finish();
	if (
		!isRecord(cursor) ||
		!isNullableString(cursor.sessionId) ||
		cursor.schemaEpoch !== SNAPSHOT_SCHEMA_EPOCH ||
		typeof cursor.updatedAt !== 'number'
	) {
		throw new BootStateDecodeError('SHELL cursor is invalid');
	}
	return {entries, cursor: cursor as unknown as StateSyncCursor};
}

function decodeMentions(source: ByteSource): Record<string, number> {
	const mentions = parseJson(source.string(), 'MENTIONS');
	source.finish();
	if (!isRecord(mentions) || Object.values(mentions).some((count) => typeof count !== 'number')) {
		throw new BootStateDecodeError('MENTIONS is malformed');
	}
	return mentions as Record<string, number>;
}

function decodeMessages(source: ByteSource): Array<BootStateMessageWindow> {
	const windows: Array<BootStateMessageWindow> = [];
	const channelCount = source.count();
	for (let channelIndex = 0; channelIndex < channelCount; channelIndex += 1) {
		const channelId = source.string();
		const flags = source.u8();
		const messageCount = source.count();
		const messages: Array<string> = [];
		for (let messageIndex = 0; messageIndex < messageCount; messageIndex += 1) {
			messages.push(source.string());
		}
		windows.push({
			channelId,
			hasMoreBefore: (flags & MessageWindowFlag.HAS_MORE_BEFORE) !== 0,
			hasMoreAfter: (flags & MessageWindowFlag.HAS_MORE_AFTER) !== 0,
			messages,
		});
	}
	source.finish();
	return windows;
}

function decodeMembers(source: ByteSource): Array<BootStateMemberList> {
	const members = parseJson(source.string(), 'MEMBERS');
	source.finish();
	if (
		!Array.isArray(members) ||
		members.some(
			(list) =>
				!isRecord(list) ||
				typeof list.guildId !== 'string' ||
				typeof list.channelId !== 'string' ||
				!Array.isArray(list.rows) ||
				!Array.isArray(list.groups),
		)
	) {
		throw new BootStateDecodeError('MEMBERS is malformed');
	}
	return members as Array<BootStateMemberList>;
}

function isMessageTops(value: unknown): boolean {
	return value === undefined || (isRecord(value) && Object.values(value).every((top) => typeof top === 'number'));
}

function isUnreadView(value: unknown): boolean {
	return (
		value === null ||
		(isRecord(value) &&
			(value.ackMessageId === null || typeof value.ackMessageId === 'string') &&
			typeof value.lastMessageId === 'string' &&
			typeof value.unreadCount === 'number' &&
			typeof value.estimated === 'boolean' &&
			(value.oldestUnreadMessageId === null || typeof value.oldestUnreadMessageId === 'string'))
	);
}

function decodeViewport(source: ByteSource): BootStateViewport {
	const viewport = parseJson(source.string(), 'VIEWPORT');
	source.finish();
	if (
		!isRecord(viewport) ||
		!isRecord(viewport.channels) ||
		(viewport.dmProfiles !== undefined &&
			(!Array.isArray(viewport.dmProfiles) ||
				viewport.dmProfiles.length > 1 ||
				viewport.dmProfiles.some(
					(profile) =>
						!isRecord(profile) ||
						!isRecord(profile.user) ||
						typeof profile.user.id !== 'string' ||
						!isRecord(profile.user_profile),
				))) ||
		Object.values(viewport.channels).some(
			(channel) =>
				!isRecord(channel) ||
				!isUnreadView(channel.unread) ||
				!isMessageTops(channel.messageTopsPx) ||
				(channel.tailHeightPx !== undefined && typeof channel.tailHeightPx !== 'number'),
		) ||
		typeof viewport.guildListScrollTop !== 'number' ||
		typeof viewport.rendererBuild !== 'string' ||
		(viewport.userAreaOverlayHeightPx !== null && typeof viewport.userAreaOverlayHeightPx !== 'number') ||
		!Array.isArray(viewport.imageSources) ||
		viewport.imageSources.some((source) => typeof source !== 'string') ||
		!Array.isArray(viewport.codeHighlights) ||
		viewport.codeHighlights.some(
			(highlight) =>
				!Array.isArray(highlight) ||
				highlight.length !== 3 ||
				highlight.some((part: unknown) => typeof part !== 'string'),
		) ||
		!Array.isArray(viewport.markdownParses) ||
		viewport.markdownParses.some(
			(parse) =>
				!Array.isArray(parse) ||
				parse.length !== 3 ||
				typeof parse[0] !== 'number' ||
				typeof parse[1] !== 'string' ||
				!Array.isArray(parse[2]),
		)
	) {
		throw new BootStateDecodeError('VIEWPORT is malformed');
	}
	return viewport as unknown as BootStateViewport;
}

function decodeGuildReadState(source: ByteSource): Readonly<Record<string, BootStateGuildReadState>> {
	const value = parseJson(source.string(), 'GUILD_READ_STATE');
	source.finish();
	if (value === null || typeof value !== 'object' || Array.isArray(value)) {
		throw new BootStateDecodeError('GUILD_READ_STATE is malformed');
	}
	for (const row of Object.values(value)) {
		if (
			row === null ||
			typeof row !== 'object' ||
			typeof row.unread !== 'boolean' ||
			!Number.isSafeInteger(row.mentionCount) ||
			row.mentionCount < 0 ||
			(row.firstUnreadChannelId !== null && typeof row.firstUnreadChannelId !== 'string')
		) {
			throw new BootStateDecodeError('GUILD_READ_STATE is malformed');
		}
	}
	return value as Record<string, BootStateGuildReadState>;
}

export function decodeBootStatePayload(bytes: Uint8Array, expected: BootStateExpectedAccount): BootStatePayload {
	if (bytes.byteLength > BOOT_STATE_MAX_PAYLOAD_BYTES) {
		throw new BootStateDecodeError('payload exceeds the size cap');
	}
	if (bytes.byteLength < HEADER_BYTES || MAGIC.some((byte, index) => bytes[index] !== byte)) {
		throw new BootStateDecodeError('bad magic');
	}
	const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
	if (view.getUint16(4, true) !== BOOT_STATE_PAYLOAD_VERSION) {
		throw new BootStateDecodeError('unknown payload version');
	}
	if (view.getUint16(6, true) !== SNAPSHOT_SCHEMA_EPOCH) {
		throw new BootStateDecodeError('snapshot schema epoch mismatch');
	}
	const sectionCount = view.getUint16(8, true);
	const dataStart = HEADER_BYTES + sectionCount * TABLE_ENTRY_BYTES;
	if (dataStart > bytes.byteLength) {
		throw new BootStateDecodeError('section table runs out of bounds');
	}
	const sections = new Map<number, ByteSource>();
	for (let index = 0; index < sectionCount; index += 1) {
		const entry = HEADER_BYTES + index * TABLE_ENTRY_BYTES;
		const kind = view.getUint8(entry);
		const offset = view.getUint32(entry + 1, true);
		const length = view.getUint32(entry + 5, true);
		if (offset < dataStart || offset + length > bytes.byteLength) {
			throw new BootStateDecodeError('section runs out of bounds');
		}
		if (sections.has(kind)) {
			throw new BootStateDecodeError('duplicate section');
		}
		sections.set(kind, new ByteSource(bytes, offset, offset + length));
	}
	const section = (kind: number, label: string): ByteSource => {
		const source = sections.get(kind);
		if (source === undefined) {
			throw new BootStateDecodeError(`${label} is missing`);
		}
		return source;
	};
	return {
		meta: decodeMeta(section(SectionKind.META, 'META'), expected),
		shell: decodeShell(section(SectionKind.SHELL, 'SHELL')),
		mentions: decodeMentions(section(SectionKind.MENTIONS, 'MENTIONS')),
		guildReadState: decodeGuildReadState(section(SectionKind.GUILD_READ_STATE, 'GUILD_READ_STATE')),
		messages: decodeMessages(section(SectionKind.MESSAGES, 'MESSAGES')),
		members: decodeMembers(section(SectionKind.MEMBERS, 'MEMBERS')),
		viewport: decodeViewport(section(SectionKind.VIEWPORT, 'VIEWPORT')),
	};
}
