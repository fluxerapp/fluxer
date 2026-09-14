// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type React from 'react';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const expressionInfoCardRollout = vi.hoisted(() => ({enabled: true}));

vi.mock('@app/features/expressions/state/ExpressionInfoCardRollout', () => ({default: expressionInfoCardRollout}));
vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, plural: () => '', t: descriptor};
});
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@app/features/app/components/LongPressable', () => ({
	LongPressable: ({children, onLongPress}: {children?: React.ReactNode; onLongPress?: () => void}) => (
		<div data-flx="test.long-pressable">
			<button type="button" data-test-long-press="true" onClick={onLongPress} data-flx="test.long-press">
				{'press'}
			</button>
			{children}
		</div>
	),
}));
vi.mock('@app/features/emoji/components/bottomsheets/EmojiInfoBottomSheet', () => ({
	EmojiInfoBottomSheet: ({isOpen}: {isOpen: boolean}) => (
		<span data-test-emoji-sheet={String(isOpen)} data-flx="test.emoji-sheet" />
	),
}));
vi.mock('@app/features/expressions/components/bottomsheets/ExpressionInfoBottomSheet', () => ({
	ExpressionInfoBottomSheet: ({isOpen}: {isOpen: boolean}) => (
		<span data-test-expression-sheet={String(isOpen)} data-flx="test.expression-sheet" />
	),
}));
vi.mock('@app/features/ui/state/MobileLayout', () => ({default: {isMobileLayout: () => true, enabled: true}}));
vi.mock('@app/features/channel/components/MessageActionUtils', () => ({
	createMessageActionHandlers: () => ({handleEmojiSelect: vi.fn()}),
	isClientSystemMessage: () => false,
	useMessagePermissions: () => ({canAddReactions: false, channel: null}),
}));
vi.mock('@app/features/messaging/hooks/useMessageReactionStore', () => ({
	useMessageReactions: () => [{emoji: {id: '30', name: 'blob', animated: false}, count: 1, me: false}],
}));
vi.mock('@app/features/messaging/hooks/useMatureMedia', () => ({
	useMatureMedia: () => ({shouldBlur: false, shouldBlock: false, canReveal: true, gateReason: 'none', reveal: vi.fn()}),
}));
vi.mock('@app/features/messaging/utils/ReactionUtils', () => ({
	getEmojiName: () => ':blob:',
	getReactionKey: () => 'reaction-key',
	useEmojiURL: () => 'https://cdn.test/blob.webp',
}));
vi.mock('@app/features/messaging/components/ReactionImage', () => ({ReactionImage: () => null}));
vi.mock('@app/features/messaging/components/popouts/ReactionTooltip', () => ({
	ReactionTooltip: ({children}: {children?: React.ReactNode}) => (
		<span data-flx="test.reaction-tooltip">{children}</span>
	),
}));
vi.mock('@app/features/emoji/components/popouts/EmojiPickerPopout', () => ({EmojiPickerPopout: () => null}));
vi.mock('@app/features/expressions/components/modals/ExpressionPickerSheet', () => ({
	ExpressionPickerSheet: () => null,
}));
vi.mock('@app/features/ui/popover/PopoverPopout', () => ({Popout: () => null, openPopout: vi.fn()}));
vi.mock('@app/features/emoji/state/Emoji', () => ({default: {getEmojiById: () => null}}));
vi.mock('@app/features/ui/commands/ContextMenuCommands', () => ({openFromEvent: vi.fn()}));
vi.mock('@app/features/ui/action_menu/items/EmojiContextMenuItems', () => ({EmojiContextMenuItems: () => null}));
vi.mock('@app/features/accessibility/state/Accessibility', () => ({default: {useReducedMotion: true}}));
vi.mock('@app/features/ui/state/KeyboardMode', () => ({default: {keyboardModeEnabled: false}}));

installVoiceMenuTestBootstrap();

const {MessageReactions} = await import('@app/features/channel/components/MessageReactions');

const message = {id: '1', channelId: '2'} as never;

let host: HTMLDivElement;
let root: Root;

function renderReactions(): void {
	act(() => {
		root.render(<MessageReactions message={message} onPopoutToggle={() => {}} data-flx="test.message-reactions" />);
	});
}

function longPress(): void {
	const trigger = host.querySelector<HTMLButtonElement>('[data-test-long-press="true"]');
	if (trigger == null) throw new Error('long press trigger is missing');
	act(() => {
		trigger.dispatchEvent(new MouseEvent('click', {bubbles: true}));
	});
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	expressionInfoCardRollout.enabled = true;
	host = document.createElement('div');
	document.body.append(host);
	root = createRoot(host);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	host.remove();
});

describe('reaction long press on the experiment arm', () => {
	it('opens the expression info sheet and never the old emoji sheet', () => {
		renderReactions();
		expect(host.querySelector('[data-test-emoji-sheet]')).toBeNull();
		longPress();
		expect(host.querySelector('[data-test-expression-sheet="true"]')).not.toBeNull();
		expect(host.querySelector('[data-test-emoji-sheet]')).toBeNull();
	});
});

describe('reaction long press on the control arm', () => {
	beforeEach(() => {
		expressionInfoCardRollout.enabled = false;
	});

	it('opens the emoji info sheet and never the expression sheet', () => {
		renderReactions();
		expect(host.querySelector('[data-test-expression-sheet]')).toBeNull();
		longPress();
		expect(host.querySelector('[data-test-emoji-sheet="true"]')).not.toBeNull();
		expect(host.querySelector('[data-test-expression-sheet]')).toBeNull();
	});
});

describe('reaction long press when the bucket flips while the sheet is open', () => {
	it('swaps the open sheet without crashing', () => {
		renderReactions();
		longPress();
		expect(host.querySelector('[data-test-expression-sheet="true"]')).not.toBeNull();
		expressionInfoCardRollout.enabled = false;
		renderReactions();
		expect(host.querySelector('[data-test-emoji-sheet="true"]')).not.toBeNull();
		expect(host.querySelector('[data-test-expression-sheet]')).toBeNull();
	});
});
