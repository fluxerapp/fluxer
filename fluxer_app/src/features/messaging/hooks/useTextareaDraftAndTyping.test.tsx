// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_STORAGE_INDEXED_DB_NAME} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import {observer} from 'mobx-react-lite';
import {act, useImperativeHandle, useRef, useState} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

vi.mock('@app/features/typing/utils/TypingUtils', () => ({
	TypingUtils: {handleComposerChange: vi.fn(), clear: vi.fn(), handleOwnMessageSent: vi.fn()},
}));

type StorageModule = typeof import('@app/features/platform/state/PersistentStorage');
type PersistenceModule = typeof import('@app/features/platform/utils/MobXPersistence');
type DraftsStore = typeof import('@app/features/messaging/state/MessagingDrafts').default;
type DraftHook = typeof import('@app/features/messaging/hooks/useTextareaDraftAndTyping').useTextareaDraftAndTyping;

interface ComposerHandle {
	type(value: string): void;
	value(): string;
}

interface Client {
	readonly storage: StorageModule;
	readonly persistence: PersistenceModule;
	readonly drafts: DraftsStore;
	readonly Composer: React.FC<{owner: string; handle: React.RefObject<ComposerHandle | null>}>;
}

const ACCOUNT_A = 'https://one.example/api::100';
const ACCOUNT_B = 'https://one.example/api::200';
const CHANNEL_ID = '1555752678099779590';
const DRAFT_FLUSH_DELAY_MS = 450;

let root: Root | null = null;
let container: HTMLElement | null = null;

function deleteAppStorageDatabase(): Promise<void> {
	return new Promise<void>((resolve, reject) => {
		const factory = getProtectedIndexedDB();
		if (factory == null) {
			resolve();
			return;
		}
		const request = factory.deleteDatabase(APP_STORAGE_INDEXED_DB_NAME);
		request.onsuccess = () => resolve();
		request.onerror = () => reject(request.error);
		request.onblocked = () => resolve();
	});
}

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function loadClient(): Promise<Client> {
	vi.resetModules();
	const storage: StorageModule = await import('@app/features/platform/state/PersistentStorage');
	const persistence: PersistenceModule = await import('@app/features/platform/utils/MobXPersistence');
	await storage.initializeAppStorage({scoped: true});
	await storage.activateAppStorageScope(ACCOUNT_A);
	const drafts = (await import('@app/features/messaging/state/MessagingDrafts')).default;
	await persistence.awaitHydration('Drafts');
	const useTextareaDraftAndTyping: DraftHook = (await import('@app/features/messaging/hooks/useTextareaDraftAndTyping'))
		.useTextareaDraftAndTyping;
	const Composer = observer(({owner, handle}: {owner: string; handle: React.RefObject<ComposerHandle | null>}) => {
		const draft = drafts.getDraft(CHANNEL_ID);
		const [value, setValue] = useState(draft);
		const previousValueRef = useRef(value);
		const valueRef = useRef(value);
		valueRef.current = value;
		useImperativeHandle(handle, () => ({type: setValue, value: () => valueRef.current}), []);
		useTextareaDraftAndTyping({
			draftOwner: owner,
			channelId: CHANNEL_ID,
			value,
			setValue,
			draft,
			previousValueRef,
			enabled: true,
			isEditingMessageInComposer: false,
		});
		return null;
	});
	return {storage, persistence, drafts, Composer};
}

async function render(client: Client, owner: string, handle: React.RefObject<ComposerHandle | null>): Promise<void> {
	await act(async () => {
		root?.render(
			<client.Composer
				key={owner}
				owner={owner}
				handle={handle}
				data-flx="messaging.use-textarea-draft-and-typing-test.client-composer"
			/>,
		);
	});
}

async function type(handle: React.RefObject<ComposerHandle | null>, value: string): Promise<void> {
	await act(async () => {
		handle.current?.type(value);
	});
}

async function switchScope(client: Client, scope: string): Promise<void> {
	await act(async () => {
		await client.storage.activateAppStorageScope(scope);
	});
}

async function storedDrafts(client: Client, scope: string): Promise<Record<string, string>> {
	client.persistence.flushPendingPersistWrites();
	await client.storage.flushAppStorageWrites();
	const {getPersistentStorageBackend} = await import('@app/features/platform/state/PersistentStorageBackend');
	const entry = await getPersistentStorageBackend().get(scope, 'Drafts');
	if (entry?.value == null) {
		return {};
	}
	return (JSON.parse(entry.value) as {drafts?: Record<string, string>}).drafts ?? {};
}

beforeEach(async () => {
	Reflect.set(globalThis, 'IS_REACT_ACT_ENVIRONMENT', true);
	window.localStorage.clear();
	await deleteAppStorageDatabase();
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
});

afterEach(async () => {
	await act(async () => {
		root?.unmount();
	});
	container?.remove();
	root = null;
	container = null;
});

test('text typed under one account never reaches the next account on the same channel', async () => {
	const client = await loadClient();
	const handle: React.RefObject<ComposerHandle | null> = {current: null};
	await render(client, ACCOUNT_A, handle);
	await type(handle, 'draft typed by account A');

	await switchScope(client, ACCOUNT_B);
	await render(client, ACCOUNT_B, handle);

	expect(handle.current?.value()).toBe('');
	expect(client.drafts.getDraft(CHANNEL_ID)).toBe('');
	await type(handle, 'typed by account B');
	await act(() => sleep(DRAFT_FLUSH_DELAY_MS));
	expect(await storedDrafts(client, ACCOUNT_B)).toEqual({[CHANNEL_ID]: 'typed by account B'});
	expect(await storedDrafts(client, ACCOUNT_A)).toEqual({[CHANNEL_ID]: 'draft typed by account A'});

	await switchScope(client, ACCOUNT_A);
	await render(client, ACCOUNT_A, handle);
	expect(handle.current?.value()).toBe('draft typed by account A');
});

test('a composer that outlives its account neither adopts nor writes the next account drafts', async () => {
	const client = await loadClient();
	await switchScope(client, ACCOUNT_B);
	client.drafts.createDraft(CHANNEL_ID, 'draft owned by account B');
	await switchScope(client, ACCOUNT_A);
	const handle: React.RefObject<ComposerHandle | null> = {current: null};
	await render(client, ACCOUNT_A, handle);
	await type(handle, 'saved before the switch');

	await switchScope(client, ACCOUNT_B);
	expect(handle.current?.value()).toBe('saved before the switch');
	await type(handle, 'typed after the storage scope moved');
	await act(() => sleep(DRAFT_FLUSH_DELAY_MS));
	await act(async () => {
		root?.render(null);
	});

	expect(client.drafts.getDraft(CHANNEL_ID)).toBe('draft owned by account B');
	expect(await storedDrafts(client, ACCOUNT_B)).toEqual({[CHANNEL_ID]: 'draft owned by account B'});
	expect(await storedDrafts(client, ACCOUNT_A)).toEqual({[CHANNEL_ID]: 'saved before the switch'});
});
