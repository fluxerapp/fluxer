// SPDX-License-Identifier: AGPL-3.0-or-later

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const SEVERAL_PEOPLE_ARE_TYPING_DESCRIPTOR = msg({
	message: 'Several people are typing...',
	comment: 'Label in the channel and chat typing users.',
});

const TIERED_TYPING_TEXT_LOCALES: ReadonlySet<string> = new Set(['en-US', 'en-GB']);

export function getTypingTierText(i18n: I18n, typingUserCount: number): string {
	if (typingUserCount <= 4 || !TIERED_TYPING_TEXT_LOCALES.has(i18n.locale)) {
		return i18n._(SEVERAL_PEOPLE_ARE_TYPING_DESCRIPTOR);
	}
	if (typingUserCount < 10) {
		return 'A handful of keyboard warriors are assembling...';
	}
	if (typingUserCount < 15) {
		return 'A symphony of clacking keys is underway...';
	}
	if (typingUserCount < 20) {
		return "It's a full-blown typing fiesta in here";
	}
	return "Whoa, it's a typing apocalypse";
}
