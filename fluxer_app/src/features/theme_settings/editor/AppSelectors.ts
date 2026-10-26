// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_HIGHLIGHT_ATTRIBUTE} from '@app/features/theme_settings/editor/AppHighlights';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export interface SemanticAttribute {
	name: string;
	description: MessageDescriptor;
}

export const SEMANTIC_ATTRIBUTES: ReadonlyArray<SemanticAttribute> = [
	{
		name: 'data-flx-message-id',
		description: msg({
			message: 'The ID of a message.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-message-type',
		description: msg({
			message: 'The type of a message, such as DEFAULT or REPLY.',
			comment:
				'Describes a CSS attribute in the "Reference" tab of the quick CSS editor. DEFAULT and REPLY are attribute values and stay in English.',
		}),
	},
	{
		name: 'data-flx-channel-id',
		description: msg({
			message: 'The ID of the channel a message is in.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-guild-id',
		description: msg({
			message: 'The ID of the community a message or avatar belongs to.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-author-id',
		description: msg({
			message: 'The user ID of the person who sent a message.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-author-username',
		description: msg({
			message: 'The username of the person who sent a message.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-author-name',
		description: msg({
			message: 'The name shown for the person who sent a message.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-author-self',
		description: msg({
			message: 'Set on messages you sent.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-author-bot',
		description: msg({
			message: 'Set on messages from bots.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-author-webhook',
		description: msg({
			message: 'Set on messages sent by a webhook.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-reply',
		description: msg({
			message: 'Set on messages that reply to another message.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-mentioned',
		description: msg({
			message: 'Set on messages that mention you.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-pinned',
		description: msg({
			message: 'Set on pinned messages.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-edited',
		description: msg({
			message: 'Set on edited messages.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-blocked',
		description: msg({
			message: 'Set on messages from people you blocked.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-system',
		description: msg({
			message: 'Set on messages that Fluxer sends, such as when someone joins.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-call',
		description: msg({
			message: 'Set on messages about a call.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-compact',
		description: msg({
			message: 'Set on messages while the compact message layout is on.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-grouped',
		description: msg({
			message: 'Set on messages shown under the previous message from the same person.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-action-bar',
		description: msg({
			message: 'Set on messages that can show the bar of message actions.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-action-bar-active',
		description: msg({
			message: 'Set on a message while its bar of message actions is showing.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-action-bar-forced',
		description: msg({
			message: 'Set on a message while its bar of message actions stays open, such as while a menu is open.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-user-id',
		description: msg({
			message: 'The user ID of an avatar.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-user-username',
		description: msg({
			message: 'The username of an avatar.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-user-name',
		description: msg({
			message: 'The name shown for an avatar.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-user-self',
		description: msg({
			message: 'Set on your own avatar.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-user-bot',
		description: msg({
			message: 'Set on the avatars of bots.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-size',
		description: msg({
			message: 'The size of an avatar in pixels.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-status',
		description: msg({
			message: 'The status shown on an avatar, such as online or idle.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-mobile-status',
		description: msg({
			message: 'Set on an avatar when that person is online on a phone.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-typing',
		description: msg({
			message: 'Set on an avatar while that person is typing.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-clickable',
		description: msg({
			message: 'Set on avatars that open a profile when clicked.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
	{
		name: 'data-flx-holidays',
		description: msg({
			message: 'Set on the root element during holidays. It lists the holidays.',
			comment: 'Describes a CSS attribute in the "Reference" tab of the quick CSS editor.',
		}),
	},
];

const FLX_SCAN_CACHE_MS = 5000;
export const SELECTOR_MATCH_LIMIT = 1000;

let flxScan: {at: number; values: ReadonlyArray<string>} | null = null;

export function readOnScreenFlxValues(): ReadonlyArray<string> {
	const now = Date.now();
	if (flxScan && now - flxScan.at < FLX_SCAN_CACHE_MS) return flxScan.values;
	const values = new Set<string>();
	for (const element of document.querySelectorAll('[data-flx]')) {
		const value = element.getAttribute('data-flx');
		if (value) values.add(value);
	}
	flxScan = {at: now, values: [...values].sort()};
	return flxScan.values;
}

export interface SelectorMatches {
	elements: ReadonlyArray<Element>;
	overLimit: boolean;
}

export function findSelectorMatches(selector: string): SelectorMatches | null {
	let list: NodeListOf<Element>;
	try {
		list = document.querySelectorAll(selector);
	} catch {
		return null;
	}
	const elements: Array<Element> = [];
	for (const element of list) {
		if (element.closest(`[data-quick-css-panel], [${APP_HIGHLIGHT_ATTRIBUTE}]`)) continue;
		if (elements.length === SELECTOR_MATCH_LIMIT) return {elements, overLimit: true};
		elements.push(element);
	}
	return {elements, overLimit: false};
}
