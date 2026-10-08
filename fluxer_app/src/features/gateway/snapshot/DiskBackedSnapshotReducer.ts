// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import {
	type InMemorySnapshotStore,
	MAX_SNAPSHOT_ACCOUNT_ENTRIES,
	MAX_SNAPSHOT_EVENT_OPERATIONS,
	SnapshotCapacityError,
	snapshotOperationEntryCount,
} from '@app/features/gateway/snapshot/InMemorySnapshotStore';
import type {SnapshotDispatch} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {
	type SnapshotGuildMemberKey,
	type SnapshotGuildRow,
	type SnapshotPresenceRow,
	type SnapshotReadStateRow,
	type SnapshotRowOp,
	type SnapshotUnavailableGuildRow,
	type SnapshotUserRow,
	snapshotEntityKey,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import {SnapshotReducer} from '@app/features/gateway/snapshot/SnapshotReducer';
import type {SnapshotReducerBacking} from '@app/features/gateway/snapshot/SnapshotReducerBacking';
import type {SnapshotReducerShellState} from '@app/features/gateway/snapshot/SnapshotReducerShellState';
import type {StateSnapshotEntry} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {SnapshotWriter} from '@app/features/gateway/snapshot/SnapshotWriter';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

const HEAVY_ENTITIES = [
	'guild',
	'unavailable_guild',
	'guild_member',
	'channel',
	'read_state',
	'presence',
	'user',
	'voice_state',
] as const;
type HeavyEntity = (typeof HEAVY_ENTITIES)[number];

const MAX_FAULT_ROUNDS = HEAVY_ENTITIES.length;

interface CollectedFaults {
	readonly missed: Map<HeavyEntity, ReadonlyArray<string>>;
	readonly needsFull: Array<HeavyEntity>;
	readonly isEmpty: boolean;
}

function* parseStoredRows<K extends string, V>(
	rows: Iterable<StateSnapshotEntry>,
	keyFromStorage: (key: string) => K,
): IterableIterator<readonly [K, V]> {
	for (const row of rows) {
		yield [keyFromStorage(row.key), JSON.parse(row.value) as V];
	}
}

type UndoValue<V> = {readonly exists: false} | {readonly exists: true; readonly value: V};

class WorkingSetMap<K extends string, V> {
	private readonly rows = new Map<K, V>();
	private readonly resolved = new Set<K>();
	private fullyLoaded = false;
	private readonly missed = new Set<K>();
	private iterated = false;
	private readonly rowUndo = new Map<K, UndoValue<V>>();
	private readonly resolvedUndo = new Map<K, boolean>();
	private snapFull = false;
	private passActive = false;

	get(key: K): V | undefined {
		if (this.rows.has(key)) {
			return this.rows.get(key);
		}
		if (this.resolved.has(key) || this.fullyLoaded) {
			return undefined;
		}
		this.markMissed(key);
		return undefined;
	}

	has(key: K): boolean {
		if (this.rows.has(key)) {
			return true;
		}
		if (this.resolved.has(key) || this.fullyLoaded) {
			return false;
		}
		this.markMissed(key);
		return false;
	}

	set(key: K, value: V): void {
		this.assertRowCapacity(key);
		this.recordRowUndo(key);
		this.recordResolvedUndo(key);
		this.rows.set(key, value);
		this.markResolved(key);
	}

	delete(key: K): boolean {
		if (this.rows.has(key)) {
			this.recordRowUndo(key);
			this.recordResolvedUndo(key);
			this.rows.delete(key);
			this.markResolved(key);
			return true;
		}
		if (this.resolved.has(key) || this.fullyLoaded) {
			return false;
		}
		this.markMissed(key);
		return false;
	}

	clear(): void {
		for (const key of this.rows.keys()) {
			this.recordRowUndo(key);
		}
		for (const key of this.resolved) {
			this.recordResolvedUndo(key);
		}
		this.rows.clear();
		this.resolved.clear();
		this.fullyLoaded = true;
	}

	get size(): number {
		return this.rows.size;
	}

	keys(): MapIterator<K> {
		this.iterated = true;
		return this.rows.keys();
	}

	values(): MapIterator<V> {
		this.iterated = true;
		return this.rows.values();
	}

	entries(): MapIterator<[K, V]> {
		this.iterated = true;
		return this.rows.entries();
	}

	[Symbol.iterator](): MapIterator<[K, V]> {
		this.iterated = true;
		return this.rows[Symbol.iterator]();
	}

	forEach(callback: (value: V, key: K, map: ReadonlyMap<K, V>) => void, thisArg?: unknown): void {
		this.iterated = true;
		for (const [key, value] of this.rows) {
			callback.call(thisArg, value, key, this);
		}
	}

	beginPass(): void {
		if (this.passActive) {
			throw new Error('Snapshot working-set pass began before the previous pass completed');
		}
		this.rowUndo.clear();
		this.resolvedUndo.clear();
		this.snapFull = this.fullyLoaded;
		this.missed.clear();
		this.iterated = false;
		this.passActive = true;
	}

	commitPass(): void {
		if (!this.passActive) {
			throw new Error('Snapshot working-set pass committed without an active pass');
		}
		this.passActive = false;
		this.clearUndo();
	}

	restore(): void {
		if (!this.passActive) {
			throw new Error('Snapshot working-set restore requested without an active pass');
		}
		for (const [key, previous] of this.rowUndo) {
			if (previous.exists) {
				this.rows.set(key, previous.value);
			} else {
				this.rows.delete(key);
			}
		}
		for (const [key, wasResolved] of this.resolvedUndo) {
			if (wasResolved) {
				this.resolved.add(key);
			} else {
				this.resolved.delete(key);
			}
		}
		this.fullyLoaded = this.snapFull;
		this.passActive = false;
		this.clearUndo();
	}

	collectMissed(): Array<K> {
		return Array.from(this.missed);
	}

	trackedFaultKeyCount(): number {
		let count = this.resolved.size;
		for (const key of this.missed) {
			if (!this.resolved.has(key)) {
				count += 1;
			}
		}
		return count;
	}

	needsFullLoad(): boolean {
		return this.iterated && !this.fullyLoaded;
	}

	applyLoadedRow(key: K, serialized: string | null): void {
		if (serialized != null) {
			this.assertRowCapacity(key);
			this.rows.set(key, JSON.parse(serialized) as V);
		}
		this.markResolved(key);
	}

	applyFullLoad(entries: Iterable<readonly [K, V]>): void {
		for (const [key, value] of entries) {
			if (!this.resolved.has(key)) {
				this.assertRowCapacity(key);
				this.rows.set(key, value);
			}
		}
		this.fullyLoaded = true;
	}

	clearBurst(): void {
		if (this.passActive) {
			throw new Error('Snapshot working set cleared during an active pass');
		}
		this.rows.clear();
		this.resolved.clear();
		this.fullyLoaded = false;
		this.missed.clear();
		this.iterated = false;
		this.clearUndo();
	}

	private recordRowUndo(key: K): void {
		if (!this.passActive || this.rowUndo.has(key)) {
			return;
		}
		if (this.rows.has(key)) {
			this.rowUndo.set(key, {exists: true, value: this.rows.get(key)!});
		} else {
			this.rowUndo.set(key, {exists: false});
		}
	}

	private assertRowCapacity(key: K): void {
		if (!this.rows.has(key) && this.rows.size >= MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
			throw new SnapshotCapacityError(`Snapshot working set exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} resident rows`);
		}
	}

	private markMissed(key: K): void {
		if (!this.missed.has(key) && this.missed.size >= MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
			throw new SnapshotCapacityError(`Snapshot working set exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} unresolved keys`);
		}
		this.missed.add(key);
	}

	private markResolved(key: K): void {
		if (!this.resolved.has(key) && this.resolved.size >= MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
			throw new SnapshotCapacityError(`Snapshot working set exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} resolved keys`);
		}
		this.resolved.add(key);
	}

	private recordResolvedUndo(key: K): void {
		if (!this.passActive || this.resolvedUndo.has(key)) {
			return;
		}
		this.resolvedUndo.set(key, this.resolved.has(key));
	}

	private clearUndo(): void {
		this.rowUndo.clear();
		this.resolvedUndo.clear();
	}
}

interface WorkingSetPassControl {
	beginPass(): void;
	commitPass(): void;
	restore(): void;
	collectMissed(): ReadonlyArray<string>;
	trackedFaultKeyCount(): number;
	needsFullLoad(): boolean;
	clearBurst(): void;
}

interface WorkingSetEntity {
	readonly control: WorkingSetPassControl;
	applyLoadedRow(key: string, serialized: string | null): void;
	applyFullLoad(rows: Iterable<StateSnapshotEntry>): void;
}

function workingSetEntity<K extends string, V>(
	map: WorkingSetMap<K, V>,
	keyFromStorage: (key: string) => K,
): WorkingSetEntity {
	return {
		control: map,
		applyLoadedRow: (key, serialized) => map.applyLoadedRow(keyFromStorage(key), serialized),
		applyFullLoad: (rows) => map.applyFullLoad(parseStoredRows<K, V>(rows, keyFromStorage)),
	};
}

class WorkingSet {
	private readonly guilds = new WorkingSetMap<string, SnapshotGuildRow>();
	private readonly unavailableGuilds = new WorkingSetMap<string, SnapshotUnavailableGuildRow>();
	private readonly guildMembers = new WorkingSetMap<SnapshotGuildMemberKey, GuildMemberData>();
	private readonly channels = new WorkingSetMap<string, WireChannel>();
	private readonly readStates = new WorkingSetMap<string, SnapshotReadStateRow>();
	private readonly presences = new WorkingSetMap<string, SnapshotPresenceRow>();
	private readonly users = new WorkingSetMap<string, SnapshotUserRow>();
	private readonly voiceStates = new WorkingSetMap<string, ReadonlyArray<VoiceState>>();
	private readonly entities: Record<HeavyEntity, WorkingSetEntity> = {
		guild: workingSetEntity(this.guilds, (key) => key),
		unavailable_guild: workingSetEntity(this.unavailableGuilds, (key) => key),
		guild_member: workingSetEntity(this.guildMembers, (key) => snapshotEntityKey('guild_member', key)),
		channel: workingSetEntity(this.channels, (key) => key),
		read_state: workingSetEntity(this.readStates, (key) => key),
		presence: workingSetEntity(this.presences, (key) => key),
		user: workingSetEntity(this.users, (key) => key),
		voice_state: workingSetEntity(this.voiceStates, (key) => key),
	};

	backing(): SnapshotReducerBacking {
		return {
			guilds: this.guilds,
			unavailableGuilds: this.unavailableGuilds,
			guildMembers: this.guildMembers,
			channels: this.channels,
			readStates: this.readStates,
			presences: this.presences,
			users: this.users,
			voiceStates: this.voiceStates,
		};
	}

	beginPass(): void {
		for (const entity of HEAVY_ENTITIES) {
			this.entities[entity].control.beginPass();
		}
	}

	commitPass(): void {
		for (const entity of HEAVY_ENTITIES) {
			this.entities[entity].control.commitPass();
		}
	}

	restore(): void {
		for (const entity of HEAVY_ENTITIES) {
			this.entities[entity].control.restore();
		}
	}

	collectFaults(): CollectedFaults {
		let trackedFaultKeyCount = 0;
		for (const entity of HEAVY_ENTITIES) {
			trackedFaultKeyCount += this.entities[entity].control.trackedFaultKeyCount();
		}
		if (trackedFaultKeyCount > MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
			throw new SnapshotCapacityError(`Snapshot working set exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} fault keys`);
		}
		const missed = new Map<HeavyEntity, ReadonlyArray<string>>();
		const needsFull: Array<HeavyEntity> = [];
		for (const entity of HEAVY_ENTITIES) {
			const map = this.entities[entity].control;
			const missedKeys = map.collectMissed();
			if (missedKeys.length > 0) {
				missed.set(entity, missedKeys);
			}
			if (map.needsFullLoad()) {
				needsFull.push(entity);
			}
		}
		return {missed, needsFull, isEmpty: missed.size === 0 && needsFull.length === 0};
	}

	loadFaults(store: InMemorySnapshotStore, storageKey: string, faults: CollectedFaults): void {
		for (const [entity, keys] of faults.missed) {
			this.loadFaultRows(store, storageKey, entity, keys);
		}
		for (const entity of faults.needsFull) {
			this.loadFullEntity(store, storageKey, entity);
		}
	}

	loadAll(store: InMemorySnapshotStore, storageKey: string): void {
		if (!store.has(storageKey)) {
			throw new Error(`Snapshot ${storageKey} is unavailable during full fault recovery`);
		}
		for (const entity of HEAVY_ENTITIES) {
			this.loadFullEntity(store, storageKey, entity);
		}
	}

	clearBurst(): void {
		for (const entity of HEAVY_ENTITIES) {
			this.entities[entity].control.clearBurst();
		}
	}

	private loadFaultRows(
		store: InMemorySnapshotStore,
		storageKey: string,
		entity: HeavyEntity,
		keys: ReadonlyArray<string>,
	): void {
		for (const key of keys) {
			this.entities[entity].applyLoadedRow(key, store.get(storageKey, entity, key));
		}
	}

	private loadFullEntity(store: InMemorySnapshotStore, storageKey: string, entity: HeavyEntity): void {
		this.entities[entity].applyFullLoad(store.entriesByPrefix(storageKey, entity, ''));
	}
}

class SnapshotOperationCollector {
	readonly operations: Array<SnapshotRowOp> = [];
	private entryCount = 0;

	emit = (op: SnapshotRowOp): void => {
		if (this.operations.length >= MAX_SNAPSHOT_EVENT_OPERATIONS) {
			throw new Error(`Snapshot event exceeded ${MAX_SNAPSHOT_EVENT_OPERATIONS} operations`);
		}
		this.entryCount += snapshotOperationEntryCount(op);
		if (this.entryCount > MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
			throw new Error(`Snapshot event exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} entries`);
		}
		this.operations.push(op);
	};
}

export interface DiskBackedSnapshotReducerOptions {
	readonly storageKey: string;
	readonly store: InMemorySnapshotStore;
	readonly writer: SnapshotWriter;
	readonly onInvalid: (error: Error) => void;
}

function snapshotReducerError(error: unknown): Error {
	if (error instanceof Error) {
		return error;
	}
	return new Error('Snapshot reducer failed with a non-error value');
}

export class DiskBackedSnapshotReducer {
	private readonly storageKey: string;
	private readonly store: InMemorySnapshotStore;
	private readonly writer: SnapshotWriter;
	private readonly onInvalid: (error: Error) => void;
	private readonly workingSet = new WorkingSet();
	private readonly reducer: SnapshotReducer;
	private invalidationError: Error | null = null;
	private disposed = false;

	constructor(options: DiskBackedSnapshotReducerOptions) {
		this.storageKey = options.storageKey;
		this.store = options.store;
		this.writer = options.writer;
		this.onInvalid = options.onInvalid;
		this.reducer = new SnapshotReducer(this.workingSet.backing());
	}

	setShellState(state: SnapshotReducerShellState): void {
		this.reducer.importShellState(state);
	}

	takeShellState(): SnapshotReducerShellState {
		return this.reducer.exportShellState();
	}

	applyReady(data: ReadyPayload): void {
		this.runGuarded(() => {
			const collector = new SnapshotOperationCollector();
			const cursor = this.reducer.applyReady(collector.emit, data);
			this.commit(collector.operations);
			this.writer.setCursor(cursor);
			this.workingSet.clearBurst();
		});
	}

	applyDispatch(dispatch: SnapshotDispatch): void {
		this.runGuarded(() => this.runPrimed(dispatch));
	}

	flush(): Promise<void> {
		if (this.invalidationError != null) {
			return Promise.reject(this.invalidationError);
		}
		return this.writer.flush();
	}

	dispose(): void {
		this.disposed = true;
		this.workingSet.clearBurst();
	}

	private runGuarded(run: () => void): void {
		if (this.disposed || this.invalidationError != null) {
			return;
		}
		try {
			run();
		} catch (error) {
			this.invalidate(snapshotReducerError(error));
		}
	}

	private applyPass(dispatch: SnapshotDispatch): SnapshotOperationCollector {
		this.workingSet.beginPass();
		const collector = new SnapshotOperationCollector();
		try {
			this.reducer.applyDispatch(collector.emit, dispatch);
		} catch (error) {
			this.workingSet.restore();
			throw error;
		}
		return collector;
	}

	private finishPass(collector: SnapshotOperationCollector): void {
		this.workingSet.commitPass();
		this.commit(collector.operations);
		this.workingSet.clearBurst();
	}

	private runPrimed(dispatch: SnapshotDispatch): void {
		for (let round = 0; round < MAX_FAULT_ROUNDS; round += 1) {
			const collector = this.applyPass(dispatch);
			let faults: CollectedFaults;
			try {
				faults = this.workingSet.collectFaults();
			} catch (error) {
				this.workingSet.restore();
				throw error;
			}
			if (faults.isEmpty) {
				this.finishPass(collector);
				return;
			}
			this.workingSet.restore();
			this.workingSet.loadFaults(this.store, this.storageKey, faults);
		}

		this.workingSet.loadAll(this.store, this.storageKey);
		this.finishPass(this.applyPass(dispatch));
	}

	private commit(operations: ReadonlyArray<SnapshotRowOp>): void {
		for (const operation of operations) {
			this.writer.enqueue(operation);
		}
	}

	private invalidate(error: Error): void {
		if (this.invalidationError != null) {
			return;
		}
		this.invalidationError = error;
		this.workingSet.clearBurst();
		this.store.evict(this.storageKey);
		this.onInvalid(error);
	}
}
