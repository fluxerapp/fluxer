// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useCustomStatusDraft} from '@app/features/user/hooks/useCustomStatusDraft';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({update: vi.fn(), limited: vi.fn(() => false), developer: {isDeveloper: false}}));
vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({i18n: {locale: 'en', _: (d: {message: string}) => d.message}}),
}));
vi.mock('@app/features/app/config/TimeWindowPresets', () => ({
	getTimeWindowPresets: ({includeDeveloperOptions}: {includeDeveloperOptions: boolean}) => [
		{key: 'never', minutes: null},
		...(includeDeveloperOptions ? [{key: 'minute', minutes: 1}] : []),
		{key: 'half_hour', minutes: 30},
	],
	TIME_WINDOW_LABEL_MESSAGES: {
		never: {message: 'Never'},
		minute: {message: 'One minute'},
		half_hour: {message: '30 minutes'},
	},
}));
vi.mock('@app/features/app/hooks/useShouldAnimate', () => ({useShouldAnimate: () => false}));
vi.mock('@app/features/devtools/state/DeveloperMode', () => ({default: mocks.developer}));
vi.mock('@app/features/emoji/state/Emoji', () => ({default: {getEmojiById: () => null}}));
vi.mock('@app/features/expressions/utils/CustomEmojiImageUrl', () => ({buildCustomEmojiURL: () => ''}));
vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({getEmojiURL: () => null}));
vi.mock('@app/features/expressions/utils/SkinToneUtils', () => ({getSkinTonedSurrogate: () => '🙂'}));
vi.mock('@app/features/user/commands/UserSettingsCommands', () => ({update: mocks.update}));
vi.mock('@app/features/user/utils/AccountLimitUtils', () => ({handleAccountLimitedError: mocks.limited}));

let root: Root;
let node: HTMLDivElement;
let draft: ReturnType<typeof useCustomStatusDraft>;
let expiresAt: string | null;
const saved = vi.fn();
function Probe({initialStatus = null}: {initialStatus?: CustomStatus | null}) {
	draft = useCustomStatusDraft({initialStatus, onSaved: saved, getExpiresAt: () => expiresAt});
	return null;
}
beforeEach(async () => {
	vi.clearAllMocks();
	mocks.developer.isDeveloper = false;
	expiresAt = null;
	node = document.createElement('div');
	document.body.append(node);
	root = createRoot(node);
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	await act(async () => {
		root.render(<Probe data-flx="user.use-custom-status-draft-test.probe" />);
	});
});
afterEach(async () => {
	await act(async () => root.unmount());
	node.remove();
});
describe('custom status draft lifecycle', () => {
	it('reinitializes drafts on reopen without retaining edited emoji or text', async () => {
		await act(async () => {
			draft.setStatusText('edited');
			draft.handleEmojiSelect({id: 'custom', name: 'wave'} as never);
		});
		expect(draft.draftStatus?.emojiId).toBe('custom');
		const initial = {text: 'Original', emojiId: null, emojiName: '🙂', expiresAt: null};
		await act(async () => draft.resetDraft(initial));
		expect(draft.statusText).toBe('Original');
		expect(draft.draftStatus).toMatchObject({text: 'Original', emojiId: null, emojiName: '🙂'});
		await act(async () => draft.handleClearDraft());
		expect(draft.draftStatus).toBeNull();
	});
	it('saves with the current expiry and closes only after the server accepts the draft', async () => {
		let finish!: () => void;
		mocks.update.mockReturnValueOnce(
			new Promise<void>((resolve) => {
				finish = resolve;
			}),
		);
		await act(async () => draft.setStatusText('  Away  '));
		expiresAt = '2030-01-01T00:00:00.000Z';
		let pending!: Promise<void>;
		await act(async () => {
			pending = draft.handleSave();
		});
		expect(draft.isSaving).toBe(true);
		expect(saved).not.toHaveBeenCalled();
		expect(mocks.update).toHaveBeenCalledWith({customStatus: expect.objectContaining({text: 'Away', expiresAt})});
		await act(async () => draft.handleSave());
		expect(mocks.update).toHaveBeenCalledOnce();
		await act(async () => {
			finish();
			await pending;
		});
		expect(saved).toHaveBeenCalledOnce();
		expect(draft.isSaving).toBe(false);
	});
	it('keeps a rejected draft open and clears its saving state', async () => {
		const failure = new Error('limited');
		mocks.update.mockRejectedValueOnce(failure);
		mocks.limited.mockReturnValueOnce(true);
		await act(async () => draft.setStatusText('Busy'));
		await act(async () => draft.handleSave());
		expect(mocks.limited).toHaveBeenCalledWith(failure);
		expect(saved).not.toHaveBeenCalled();
		expect(draft.isSaving).toBe(false);
		expect(draft.statusText).toBe('Busy');
	});
});
