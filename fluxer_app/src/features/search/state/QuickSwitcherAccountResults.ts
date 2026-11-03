// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHeaderResult} from '@app/features/search/state/QuickSwitcherResultConverters';
import type {
	AccountChannelResult,
	AccountResult,
	QuickSwitcherResult,
} from '@app/features/search/state/QuickSwitcherTypes';
import {createSearchTerm, scoreSearchTerm} from '@app/features/search/utils/SearchTextMatching';
import {compareStrings} from '@app/lib/list/CompareStrings';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import type {
	AccountEngineAccountActivity,
	AccountEngineChannelActivity,
	AccountEngineUser,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ACCOUNT_ON_HOST_DESCRIPTOR = msg({
	message: '{accountName} on {host}',
	comment:
		'Quick switcher section header and row label for another signed-in account when accounts span several instances. accountName is the account display name and host is the instance host name.',
});
const MORE_IN_ACCOUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# more in {accountName}} other {# more in {accountName}}}',
	comment:
		'Last quick switcher row in another account section. count is how many more unread or mentioned channels that account has. accountName is the account display name.',
});
const COMMUNITY_ON_ACCOUNT_DESCRIPTOR = msg({
	message: '{communityName} · {accountName}',
	comment:
		'Quick switcher row subtitle for a channel on another account. communityName is the community name and accountName is the account display name. Keep the middle dot between the two names.',
});
const ACCOUNT_CHANNEL_LABEL_DESCRIPTOR = msg({
	message: '{channelName} in {communityName} on {accountName}',
	comment:
		'Accessible name for a quick switcher row that opens a channel on another account. channelName, communityName and accountName are names.',
});
const ACCOUNT_DM_LABEL_DESCRIPTOR = msg({
	message: '{channelName} on {accountName}',
	comment:
		'Accessible name for a quick switcher row that opens a direct message on another account. channelName is the conversation name and accountName is the account display name.',
});
const UNNAMED_CHANNEL_DESCRIPTOR = msg({
	message: 'Unnamed channel',
	comment: 'Fallback name for a channel on another account when its name is not known yet.',
});
const SESSION_EXPIRED_DESCRIPTOR = msg({
	message: 'Session expired',
	comment: 'Quick switcher row subtitle for another account whose session expired and needs signing in again.',
});
const SWITCH_TO_ACCOUNT_DESCRIPTOR = msg({
	message: 'Switch to this account',
	comment: 'Quick switcher row subtitle for a row that switches to another signed-in account.',
});

const OTHER_ACCOUNT_SECTION_ROW_LIMIT = 5;
export const OTHER_ACCOUNT_ACTIVITY_CHANNEL_LIMIT = OTHER_ACCOUNT_SECTION_ROW_LIMIT + 1;
const ACCOUNT_PRIORITY_SCORE = 7;
const QUERY_ACCOUNT_CHANNEL_LIMIT = 20;
const TITLE_MATCH_WEIGHT = 2;

export interface OtherAccountInfo {
	readonly accountKey: string;
	readonly displayName: string;
	readonly username: string | null;
	readonly host: string | null;
	readonly isExpired: boolean;
	readonly mentionCount: number;
	readonly hasUnread: boolean;
	readonly lastActivityAt: number | null;
}

export interface OtherAccountsContext {
	readonly accounts: ReadonlyArray<OtherAccountInfo>;
	readonly activity: ReadonlyMap<string, AccountEngineAccountActivity>;
	readonly showHost: boolean;
}

export interface OtherAccountQueryResults {
	readonly leading: Array<QuickSwitcherResult>;
	readonly trailing: Array<QuickSwitcherResult>;
}

function accountLabel(account: OtherAccountInfo, showHost: boolean, i18n: I18n): string {
	if (!showHost || account.host == null) return account.displayName;
	return i18n._(ACCOUNT_ON_HOST_DESCRIPTOR, {accountName: account.displayName, host: account.host});
}

function userName(user: AccountEngineUser): string {
	return user.globalName != null && user.globalName.length > 0 ? user.globalName : user.username;
}

function isDirectChannel(channel: AccountEngineChannelActivity): boolean {
	return channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
}

export function accountChannelTitle(channel: AccountEngineChannelActivity, i18n: I18n): string {
	if (channel.name != null && channel.name.length > 0) return channel.name;
	if (channel.recipients.length > 0) return channel.recipients.map(userName).join(', ');
	return i18n._(UNNAMED_CHANNEL_DESCRIPTOR);
}

export function accountChannelAccessibleName(result: AccountChannelResult, i18n: I18n): string {
	if (result.guildName == null) {
		return i18n._(ACCOUNT_DM_LABEL_DESCRIPTOR, {channelName: result.title, accountName: result.accountName});
	}
	return i18n._(ACCOUNT_CHANNEL_LABEL_DESCRIPTOR, {
		channelName: result.title,
		communityName: result.guildName,
		accountName: result.accountName,
	});
}

function accountChannelSubtitle(
	account: OtherAccountInfo,
	guildName: string | null,
	underAccountHeader: boolean,
	i18n: I18n,
): string | undefined {
	if (underAccountHeader) return guildName ?? undefined;
	if (guildName == null) return account.displayName;
	return i18n._(COMMUNITY_ON_ACCOUNT_DESCRIPTOR, {communityName: guildName, accountName: account.displayName});
}

function toAccountChannelResult(
	account: OtherAccountInfo,
	channel: AccountEngineChannelActivity,
	underAccountHeader: boolean,
	i18n: I18n,
): AccountChannelResult {
	const guildName = isDirectChannel(channel) ? null : channel.guildName;
	return {
		type: QuickSwitcherResultTypes.ACCOUNT_CHANNEL,
		id: `${account.accountKey}|${channel.channelId}`,
		title: accountChannelTitle(channel, i18n),
		subtitle: accountChannelSubtitle(account, guildName, underAccountHeader, i18n),
		accountKey: account.accountKey,
		accountName: account.displayName,
		channelId: channel.channelId,
		guildId: channel.guildId,
		guildName,
		channelType: channel.type,
		mentionCount: channel.mentionCount,
		unread: channel.unread || channel.mentionCount > 0,
	};
}

function toAccountResult(
	account: OtherAccountInfo,
	showHost: boolean,
	i18n: I18n,
	moreCount: number | null = null,
): AccountResult {
	const title =
		moreCount == null
			? accountLabel(account, showHost, i18n)
			: i18n._(MORE_IN_ACCOUNT_DESCRIPTOR, {count: moreCount, accountName: account.displayName});
	let subtitle: string | undefined;
	if (moreCount == null) {
		subtitle = account.isExpired
			? i18n._(SESSION_EXPIRED_DESCRIPTOR)
			: (account.username ?? i18n._(SWITCH_TO_ACCOUNT_DESCRIPTOR));
	}
	return {
		type: QuickSwitcherResultTypes.ACCOUNT,
		id: moreCount == null ? account.accountKey : `${account.accountKey}|more`,
		title,
		subtitle,
		accountKey: account.accountKey,
		accountName: account.displayName,
		mentionCount: account.mentionCount,
		hasUnread: account.hasUnread,
		moreCount,
	};
}

function sortChannelsForSection(
	channels: ReadonlyArray<AccountEngineChannelActivity>,
): Array<AccountEngineChannelActivity> {
	const mentioned = channels.filter((channel) => channel.mentionCount > 0);
	const unread = channels.filter((channel) => channel.mentionCount === 0 && channel.unread);
	return [...mentioned, ...unread];
}

function newestActivityFirst(left: OtherAccountInfo, right: OtherAccountInfo): number {
	const leftAt = left.lastActivityAt ?? 0;
	const rightAt = right.lastActivityAt ?? 0;
	if (leftAt !== rightAt) return rightAt - leftAt;
	if (left.mentionCount !== right.mentionCount) return right.mentionCount - left.mentionCount;
	return compareStrings(left.accountKey, right.accountKey);
}

export function generateOtherAccountSections(context: OtherAccountsContext, i18n: I18n): Array<QuickSwitcherResult> {
	const results: Array<QuickSwitcherResult> = [];
	for (const account of [...context.accounts].sort(newestActivityFirst)) {
		const activity = context.activity.get(account.accountKey);
		if (activity == null) continue;
		const channels = sortChannelsForSection(activity.channels);
		if (channels.length === 0) continue;
		const shown = channels.slice(0, OTHER_ACCOUNT_SECTION_ROW_LIMIT);
		results.push(
			createHeaderResult(`account-${account.accountKey}`, accountLabel(account, context.showHost, i18n), {
				accountKey: account.accountKey,
			}),
		);
		for (const channel of shown) results.push(toAccountChannelResult(account, channel, true, i18n));
		const total = Math.max(activity.totalChannelCount, channels.length);
		const moreCount = total - shown.length;
		if (moreCount > 0) results.push(toAccountResult(account, context.showHost, i18n, moreCount));
	}
	return results;
}

function bestScore(values: ReadonlyArray<string | null>, query: string): number {
	const term = createSearchTerm(query.toLocaleLowerCase());
	let best = 0;
	for (const value of values) {
		if (value == null || value.length === 0) continue;
		best = Math.max(best, scoreSearchTerm(value, term, false));
	}
	return best;
}

export function searchOtherAccounts(
	query: string,
	context: OtherAccountsContext,
	i18n: I18n,
): OtherAccountQueryResults {
	const trimmed = query.trim();
	if (trimmed.length === 0) return {leading: [], trailing: []};
	const leading: Array<{result: AccountResult; score: number}> = [];
	const trailingAccounts: Array<{result: AccountResult; score: number}> = [];
	const channelMatches: Array<{result: AccountChannelResult; score: number}> = [];
	for (const account of context.accounts) {
		const nameScore = bestScore([account.displayName, account.username], trimmed);
		const score = Math.max(nameScore, bestScore([account.host], trimmed));
		if (score > 0) {
			const entry = {result: toAccountResult(account, context.showHost, i18n), score};
			if (nameScore >= ACCOUNT_PRIORITY_SCORE) leading.push(entry);
			else trailingAccounts.push(entry);
		}
		const activity = context.activity.get(account.accountKey);
		if (activity == null) continue;
		for (const channel of activity.channels) {
			const title = accountChannelTitle(channel, i18n);
			const channelScore = Math.max(
				bestScore([title], trimmed) * TITLE_MATCH_WEIGHT,
				bestScore(
					[channel.guildName, ...channel.recipients.flatMap((user) => [user.globalName, user.username])],
					trimmed,
				),
			);
			if (channelScore > 0)
				channelMatches.push({result: toAccountChannelResult(account, channel, false, i18n), score: channelScore});
		}
	}
	const byScore = <T extends {result: {mentionCount: number; title: string}; score: number}>(left: T, right: T) => {
		if (left.score !== right.score) return right.score - left.score;
		if (left.result.mentionCount !== right.result.mentionCount)
			return right.result.mentionCount - left.result.mentionCount;
		return left.result.title.localeCompare(right.result.title);
	};
	return {
		leading: leading.sort(byScore).map((entry) => entry.result),
		trailing: [
			...trailingAccounts.sort(byScore).map((entry) => entry.result),
			...channelMatches
				.sort(byScore)
				.slice(0, QUERY_ACCOUNT_CHANNEL_LIMIT)
				.map((entry) => entry.result),
		],
	};
}

export function listOtherAccountResults(context: OtherAccountsContext, i18n: I18n): Array<AccountResult> {
	return [...context.accounts]
		.sort(newestActivityFirst)
		.map((account) => toAccountResult(account, context.showHost, i18n));
}

export function mergeWithOtherAccountResults(
	currentAccountResults: ReadonlyArray<QuickSwitcherResult>,
	otherAccountResults: OtherAccountQueryResults,
): Array<QuickSwitcherResult> {
	return [...otherAccountResults.leading, ...currentAccountResults, ...otherAccountResults.trailing];
}
