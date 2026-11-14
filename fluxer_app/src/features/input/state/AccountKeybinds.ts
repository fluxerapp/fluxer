// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry} from '@app/features/input/state/InputKeybind';
import {isAccountKeybindCommand, type KeybindCommand} from '@app/features/input/state/input_keybind/KeybindCommands';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

function isStoredEntry(value: unknown): value is CustomKeybindEntry {
	if (typeof value !== 'object' || value === null) return false;
	const entry = value as Partial<CustomKeybindEntry>;
	return (
		typeof entry.id === 'string' &&
		isAccountKeybindCommand(entry.action) &&
		typeof entry.enabled === 'boolean' &&
		typeof entry.combo === 'object' &&
		entry.combo !== null &&
		typeof entry.combo.key === 'string'
	);
}

class AccountKeybinds {
	entries: Array<CustomKeybindEntry> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, AppStorageKey.ACCOUNT_KEYBINDS, ['entries'], {syncAcrossTabs: true});
	}

	get validEntries(): ReadonlyArray<CustomKeybindEntry> {
		return Array.isArray(this.entries) ? this.entries.filter(isStoredEntry) : [];
	}

	hasEntry(id: string): boolean {
		return this.validEntries.some((entry) => entry.id === id);
	}

	hasEntriesForAction(action: KeybindCommand): boolean {
		return this.validEntries.some((entry) => entry.action === action);
	}

	add(entry: CustomKeybindEntry): void {
		this.entries = [...this.validEntries, entry];
	}

	update(id: string, patch: Partial<Omit<CustomKeybindEntry, 'id'>>): void {
		this.entries = this.validEntries.map((entry) => (entry.id === id ? {...entry, ...patch} : entry));
	}

	remove(id: string): CustomKeybindEntry | null {
		const removed = this.validEntries.find((entry) => entry.id === id) ?? null;
		this.entries = this.validEntries.filter((entry) => entry.id !== id);
		return removed;
	}

	removeForAction(action: KeybindCommand): void {
		this.entries = this.validEntries.filter((entry) => entry.action !== action);
	}

	adopt(entries: ReadonlyArray<CustomKeybindEntry>): void {
		const adopted = entries.filter(
			(entry) => isStoredEntry(entry) && !this.hasEntriesForAction(entry.action as KeybindCommand),
		);
		if (adopted.length === 0) return;
		this.entries = [...this.validEntries, ...adopted.map((entry) => ({...entry, combo: {...entry.combo}}))];
	}

	reset(): void {
		this.entries = [];
	}
}

export default new AccountKeybinds();
