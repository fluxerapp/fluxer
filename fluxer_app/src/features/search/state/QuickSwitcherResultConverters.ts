// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	Candidate,
	HeaderResult,
	QuickSwitcherExecutableResult,
	QuickSwitcherResult,
} from '@app/features/search/state/QuickSwitcherTypes';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';

function assertNever(value: never): never {
	throw new Error(`Unexpected quick switcher type: ${String(value)}`);
}

export function candidateToResult(candidate: Candidate): QuickSwitcherExecutableResult {
	switch (candidate.type) {
		case QuickSwitcherResultTypes.USER:
			return {
				type: QuickSwitcherResultTypes.USER,
				id: candidate.id,
				title: candidate.title,
				subtitle: candidate.subtitle,
				user: candidate.user,
				dmChannelId: candidate.dmChannelId,
			};
		case QuickSwitcherResultTypes.GROUP_DM:
			return {
				type: QuickSwitcherResultTypes.GROUP_DM,
				id: candidate.id,
				title: candidate.title,
				subtitle: candidate.subtitle,
				channel: candidate.channel,
			};
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
		case QuickSwitcherResultTypes.VOICE_CHANNEL: {
			return {
				type: candidate.type,
				id: candidate.id,
				title: candidate.title,
				subtitle: candidate.subtitle,
				channel: candidate.channel,
				guild: candidate.guild,
			};
		}
		case QuickSwitcherResultTypes.GUILD:
			return {
				type: QuickSwitcherResultTypes.GUILD,
				id: candidate.id,
				title: candidate.title,
				subtitle: candidate.subtitle,
				guild: candidate.guild,
			};
		case QuickSwitcherResultTypes.VIRTUAL_GUILD:
			return {
				type: QuickSwitcherResultTypes.VIRTUAL_GUILD,
				id: candidate.id,
				title: candidate.title,
				subtitle: candidate.subtitle,
				virtualGuildType: candidate.virtualGuildType,
			};
		case QuickSwitcherResultTypes.SETTINGS:
			return {
				type: QuickSwitcherResultTypes.SETTINGS,
				id: candidate.id,
				title: candidate.title,
				subtitle: candidate.subtitle,
				settingsTab: candidate.settingsTab,
				settingsSubtab: candidate.settingsSubtab,
			};
		default:
			return assertNever(candidate);
	}
}

export function createHeaderResult(id: string, title: string, options: {accountKey?: string} = {}): HeaderResult {
	if (options.accountKey == null) return {type: QuickSwitcherResultTypes.HEADER, id, title};
	return {type: QuickSwitcherResultTypes.HEADER, id, title, accountKey: options.accountKey};
}

export function getFirstSelectableIndex(results: ReadonlyArray<QuickSwitcherResult>): number {
	for (let i = 0; i < results.length; i += 1) {
		if (results[i].type !== QuickSwitcherResultTypes.HEADER) {
			return i;
		}
	}
	return -1;
}
