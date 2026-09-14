// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageViewContextProvider} from '@app/features/channel/components/MessageViewContext';
import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type React from 'react';
import {act, cloneElement, useState} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const matureMedia = vi.hoisted(() => ({blurred: false, canReveal: true, blocked: false}));
const expressionInfoCardRollout = vi.hoisted(() => ({enabled: true}));

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: React.ReactNode}) => <span data-flx="test.trans">{children}</span>,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@app/features/expressions/state/ExpressionInfoCardRollout', () => ({default: expressionInfoCardRollout}));
vi.mock('@app/features/messaging/hooks/useMatureMedia', () => ({
	useMatureMedia: () => {
		const [isRevealed, setIsRevealed] = useState(false);
		return {
			shouldBlur: matureMedia.blurred && !isRevealed,
			shouldBlock: matureMedia.blocked,
			canReveal: matureMedia.canReveal,
			gateReason: 'none',
			reveal: () => {
				if (matureMedia.canReveal) setIsRevealed(true);
			},
		};
	},
}));
vi.mock('@app/features/ui/popover/PopoverPopout', () => ({
	openPopout: vi.fn(),
	Popout: ({
		children,
		render,
		tooltip,
		shouldOpenOnClick,
	}: {
		children: React.ReactElement<{onClick?: (event: React.MouseEvent<HTMLElement>) => void}>;
		render?: (props: {popoutKey: number; onClose: () => void}) => React.ReactNode;
		tooltip?: string | (() => React.ReactNode);
		shouldOpenOnClick?: (event: React.MouseEvent<HTMLElement>) => boolean;
	}) => {
		const [isOpen, setIsOpen] = useState(false);
		return (
			<span data-test-popout="true" data-flx="test.popout">
				{cloneElement(children, {
					onClick: (event: React.MouseEvent<HTMLElement>) => {
						if (shouldOpenOnClick?.(event) ?? true) setIsOpen(true);
					},
				})}
				<span data-test-popout-tooltip="true" data-flx="test.popout-tooltip">
					{typeof tooltip === 'function' ? tooltip() : tooltip}
				</span>
				{isOpen ? (
					<span data-test-popout-card="true" data-flx="test.popout-card">
						{render?.({popoutKey: 1, onClose: () => setIsOpen(false)})}
					</span>
				) : null}
			</span>
		);
	},
}));
vi.mock('@app/features/expressions/components/ExpressionInfoCard', () => ({
	ExpressionInfoCard: ({displayName}: {displayName: string}) => (
		<span data-test-info-card={displayName} data-flx="test.info-card" />
	),
}));
vi.mock('@app/features/expressions/components/bottomsheets/ExpressionInfoBottomSheet', () => ({
	ExpressionInfoBottomSheet: ({isOpen}: {isOpen: boolean}) => (
		<span data-test-bottom-sheet={String(isOpen)} data-flx="test.bottom-sheet" />
	),
}));
vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {updatePresence: vi.fn()},
	setLocalPresenceUserSettings: vi.fn(),
	ACCOUNT_PRESENCE_INTENT_MAX_AGE_MS: 60_000,
}));
vi.mock('@app/features/ui/commands/ContextMenuCommands', () => ({openFromEvent: vi.fn()}));
vi.mock('@app/features/ui/action_menu/MessageContextMenu', () => ({MessageContextMenu: () => null}));
vi.mock('@app/features/ui/action_menu/items/StickerContextMenuItems', () => ({StickerInlineMenuItems: () => null}));
vi.mock('@app/features/channel/components/MessageReactions', () => ({MessageReactions: () => null}));
vi.mock('@app/features/channel/components/embeds/ChannelEmbed', () => ({Embed: () => null}));
vi.mock('@app/features/channel/components/embeds/attachments/Attachment', () => ({Attachment: () => null}));
vi.mock('@app/features/channel/components/embeds/attachments/AttachmentMosaic', () => ({AttachmentMosaic: () => null}));
vi.mock('@app/features/channel/components/InviteEmbed', () => ({InviteEmbed: () => null}));
vi.mock('@app/features/channel/components/GiftEmbed', () => ({GiftEmbed: () => null}));
vi.mock('@app/features/channel/components/ThemeEmbed', () => ({ThemeEmbed: () => null}));
vi.mock('@app/features/messaging/components/markdown', () => ({SafeMarkdown: () => null}));
vi.mock('@app/features/channel/components/TimestampWithTooltip', () => ({TimestampWithTooltip: () => null}));
vi.mock('@app/features/ui/components/Avatar', () => ({Avatar: () => null}));
vi.mock('@app/features/app/components/shared/GroupDMAvatar', () => ({GroupDMAvatar: () => null}));
vi.mock('@app/features/app/components/shared/SpoilerOverlay', () => ({SpoilerOverlay: () => null}));
vi.mock('@app/features/messaging/utils/MessageNavigator', () => ({goToMessage: vi.fn()}));
vi.mock('@app/features/messaging/utils/MessageCopyTextUtils', () => ({buildMessageSnapshotCopyText: () => ''}));

installVoiceMenuTestBootstrap();

const {MessageAttachments} = await import('@app/features/channel/components/MessageAttachments');

const STICKER_NAME = 'blobsticker';
const message = {
	id: '1',
	channelId: '2',
	content: '',
	attachments: [],
	embeds: [],
	invites: [],
	themes: [],
	gifts: [],
	stickers: [{id: '20', name: STICKER_NAME, format_type: 1}],
	messageSnapshots: null,
	suppressEmbeds: false,
};

let host: HTMLDivElement;
let root: Root;

function renderMessage(): void {
	act(() => {
		root.render(
			<MessageViewContextProvider
				value={
					{
						channel: {id: '2'},
						message,
						shouldGroup: false,
						isHovering: false,
						messageDisplayCompact: false,
						handleDelete: () => {},
					} as never
				}
				data-flx="test.message-view-context-provider"
			>
				<MessageAttachments data-flx="test.message-attachments" />
			</MessageViewContextProvider>,
		);
	});
}

function stickerButton(): HTMLButtonElement {
	const button = host.querySelector<HTMLButtonElement>('button[data-message-sticker="true"]');
	if (button == null) throw new Error('sticker trigger is missing');
	return button;
}

function clickSticker(): void {
	const button = stickerButton();
	act(() => {
		button.dispatchEvent(new MouseEvent('click', {bubbles: true}));
	});
}

function isBlurred(): boolean {
	return host.querySelector('img')?.className.includes('matureStickerBlurred') === true;
}

function isCardOpen(): boolean {
	return host.querySelector('[data-test-popout-card]') != null;
}

function tooltipText(): string {
	return host.querySelector('[data-test-popout-tooltip]')?.textContent ?? '';
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	matureMedia.blurred = false;
	matureMedia.canReveal = true;
	matureMedia.blocked = false;
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

describe('message stickers', () => {
	it('renders the sticker as an info card trigger', () => {
		renderMessage();
		expect(host.querySelector('[data-test-popout]')).not.toBeNull();
		expect(stickerButton().getAttribute('aria-label')).toBe(STICKER_NAME);
		expect(isCardOpen()).toBe(false);
	});

	it('opens the info card on the first activation when nothing is blurred', () => {
		renderMessage();
		clickSticker();
		expect(isCardOpen()).toBe(true);
		expect(host.querySelector(`[data-test-info-card="${STICKER_NAME}"]`)).not.toBeNull();
	});

	it('reveals instead of opening the card while the sticker is blurred', () => {
		matureMedia.blurred = true;
		renderMessage();
		expect(isBlurred()).toBe(true);
		clickSticker();
		expect(isBlurred()).toBe(false);
		expect(isCardOpen()).toBe(false);
	});

	it('opens the info card once the blurred sticker has been revealed', () => {
		matureMedia.blurred = true;
		renderMessage();
		clickSticker();
		clickSticker();
		expect(isCardOpen()).toBe(true);
	});

	it('neither reveals nor opens the card when the blur cannot be revealed', () => {
		matureMedia.blurred = true;
		matureMedia.canReveal = false;
		renderMessage();
		clickSticker();
		expect(isBlurred()).toBe(true);
		expect(isCardOpen()).toBe(false);
	});

	it('hides the card hint from the hover tooltip while the sticker is blurred', () => {
		matureMedia.blurred = true;
		renderMessage();
		expect(tooltipText()).toBe(STICKER_NAME);
		clickSticker();
		expect(tooltipText()).toContain(STICKER_NAME);
		expect(tooltipText()).toContain('Click to learn more');
	});
});

describe('message stickers on the control arm', () => {
	beforeEach(() => {
		expressionInfoCardRollout.enabled = false;
	});

	it('renders the sticker inside the plain tooltip with no info card trigger', () => {
		renderMessage();
		expect(host.querySelector('[data-test-popout]')).toBeNull();
		expect(stickerButton().getAttribute('aria-label')).toBe(STICKER_NAME);
		expect(host.querySelector('[data-test-info-card]')).toBeNull();
	});

	it('opens no info card when the sticker is activated', () => {
		renderMessage();
		clickSticker();
		expect(isCardOpen()).toBe(false);
		expect(host.querySelector('[data-test-info-card]')).toBeNull();
	});

	it('still reveals a blurred sticker on activation', () => {
		matureMedia.blurred = true;
		renderMessage();
		expect(isBlurred()).toBe(true);
		clickSticker();
		expect(isBlurred()).toBe(false);
		expect(isCardOpen()).toBe(false);
	});

	it('keeps the context menu attributes on the trigger', () => {
		renderMessage();
		expect(stickerButton().getAttribute('data-message-sticker')).toBe('true');
	});
});
