// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {act, createElement, type ReactNode} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/expressions/utils/CustomEmojiImageUrl', () => ({
	buildCustomEmojiURL: ({id, animated}: {id: string; animated: boolean}) =>
		`https://cdn.test/emojis/${id}.${animated ? 'gif' : 'png'}`,
}));
vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({getEmojiURL: () => 'https://cdn.test/twemoji.svg'}));
vi.mock('@app/features/expressions/utils/UnicodeEmojis', () => ({
	default: {
		normalizeEmojiNameToSurrogate: (value: string) => value,
		nameForSurrogate: (_surrogate: string, _colons: boolean, fallback: string) => fallback,
	},
}));
vi.mock('@app/features/emoji/state/Emoji', () => ({default: {getEmojiById: () => null}}));
vi.mock('@app/features/ui/state/MobileLayout', () => ({default: {enabled: false}}));
vi.mock('@app/features/app/hooks/useShouldAnimate', () => ({useShouldAnimate: () => true}));
vi.mock('@app/features/presence/hooks/usePresenceCustomStatus', () => ({usePresenceCustomStatus: () => null}));
vi.mock('@app/features/ui/hooks/useTextOverflow', () => ({useTextOverflow: () => false}));
vi.mock('@app/features/theme/layout/RemFromPx', () => ({remFromPx: (value: number) => value}));
vi.mock('@app/features/ui/focus_ring/FocusRing', () => ({
	default: ({children}: {children?: ReactNode}) => children,
}));
vi.mock('@lingui/react/macro', () => ({Trans: ({children}: {children?: ReactNode}) => children}));
vi.mock('@phosphor-icons/react', () => ({
	PencilIcon: () => createElement('span'),
	SmileyIcon: () => createElement('span'),
}));
vi.mock('@app/features/ui/tooltip/Tooltip', () => ({
	Tooltip: ({text, children}: {text?: unknown; children?: ReactNode}) =>
		createElement('div', {'data-simple-tooltip': String(text ?? '')}, children),
}));

const {CustomStatusDisplay} = await import(
	'@app/features/app/components/shared/custom_status_display/CustomStatusDisplay'
);

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const STATUS = {text: null, expiresAt: null, emojiId: '4001', emojiName: 'partyblob', emojiAnimated: false};

let container: HTMLDivElement;
let root: Root;

beforeEach(() => {
	container = document.createElement('div');
	document.body.append(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => root.unmount());
	container.remove();
});

function render(): void {
	act(() => {
		root.render(createElement(CustomStatusDisplay, {customStatus: STATUS, showTooltip: true}));
	});
}

describe('CustomStatusDisplay status emoji tooltip', () => {
	it('renders the emoji name tooltip', () => {
		render();

		expect(container.querySelector('[data-simple-tooltip=":partyblob:"]')).not.toBeNull();
	});
});
