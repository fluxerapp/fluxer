// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as DiscoveryCommands from '@app/features/discovery/commands/DiscoveryCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';

const COULDN_T_JOIN_DESCRIPTOR = msg({
	message: "Couldn't join this community",
	comment: 'Title of the generic fallback error modal shown when joining a discovery community fails.',
});
const COULDN_T_JOIN_GENERIC_DESCRIPTOR = msg({
	message: 'Something went wrong. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when joining a discovery community fails.',
});
const SERVER_FULL_TITLE_DESCRIPTOR = msg({
	message: 'This community is full',
	comment: 'Title of the error modal shown when a discovery community has reached its member limit.',
});
const SERVER_FULL_MESSAGE_DESCRIPTOR = msg({
	message: "This community has reached its member limit, so you can't join right now.",
	comment: 'Body of the error modal shown when a discovery community has reached its member limit.',
});
const TOO_MANY_SERVERS_TITLE_DESCRIPTOR = msg({
	message: "You've reached the community limit",
	comment: 'Title of the error modal shown when the user is already in the maximum number of communities.',
});
const TOO_MANY_SERVERS_MESSAGE_DESCRIPTOR = msg({
	message: "You're in the maximum number of communities. Leave one and try again.",
	comment: 'Body of the error modal shown when the user is already in the maximum number of communities.',
});
const BANNED_TITLE_DESCRIPTOR = msg({
	message: "You can't join this community",
	comment: 'Title of the error modal shown when the user is banned from a discovery community.',
});
const BANNED_MESSAGE_DESCRIPTOR = msg({
	message: 'You have been banned from this community.',
	comment: 'Body of the error modal shown when the user is banned from a discovery community.',
});
const NOT_AVAILABLE_TITLE_DESCRIPTOR = msg({
	message: 'This community is no longer available',
	comment:
		'Title of the error modal shown when a discovery community is no longer joinable (delisted, invites off, or discovery disabled).',
});
const NOT_AVAILABLE_MESSAGE_DESCRIPTOR = msg({
	message: "It may have left discovery or turned off new joins. Refresh the page and you won't see it again.",
	comment:
		'Body of the error modal shown when a discovery community is no longer joinable (delisted, invites off, or discovery disabled).',
});
const GOING_TOO_FAST_TITLE_DESCRIPTOR = msg({
	message: "You're going too fast",
	comment: 'Title of the error modal shown when joining a discovery community is rate limited.',
});
const GOING_TOO_FAST_MESSAGE_DESCRIPTOR = msg({
	message: 'Please wait a moment and try again.',
	comment: 'Body of the error modal shown when joining a discovery community is rate limited.',
});

function resolveJoinGuildErrorContent(code: string | undefined): {title: string; message: string} {
	switch (code) {
		case APIErrorCodes.MAX_GUILD_MEMBERS:
			return {
				title: i18n._(SERVER_FULL_TITLE_DESCRIPTOR),
				message: i18n._(SERVER_FULL_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.MAX_GUILDS:
			return {
				title: i18n._(TOO_MANY_SERVERS_TITLE_DESCRIPTOR),
				message: i18n._(TOO_MANY_SERVERS_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.USER_BANNED_FROM_GUILD:
		case APIErrorCodes.USER_IP_BANNED_FROM_GUILD:
			return {
				title: i18n._(BANNED_TITLE_DESCRIPTOR),
				message: i18n._(BANNED_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE:
		case APIErrorCodes.DISCOVERY_DISABLED:
		case APIErrorCodes.INVITES_DISABLED:
			return {
				title: i18n._(NOT_AVAILABLE_TITLE_DESCRIPTOR),
				message: i18n._(NOT_AVAILABLE_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.RATE_LIMITED:
			return {
				title: i18n._(GOING_TOO_FAST_TITLE_DESCRIPTOR),
				message: i18n._(GOING_TOO_FAST_MESSAGE_DESCRIPTOR),
			};
		default:
			return {
				title: i18n._(COULDN_T_JOIN_DESCRIPTOR),
				message: i18n._(COULDN_T_JOIN_GENERIC_DESCRIPTOR),
			};
	}
}

function showJoinGuildErrorModal(error: unknown): void {
	const code = failureCode(error);
	if (code === APIErrorCodes.ACCOUNT_SUSPICIOUS_ACTIVITY && (Users.currentUser?.requiredActions?.length ?? 0) > 0) {
		return;
	}
	const {title, message} = resolveJoinGuildErrorContent(code);
	ModalCommands.push(
		modal(() => <GenericErrorModal title={title} message={message} data-flx="discovery.join.generic-error-modal" />),
	);
}

export async function joinDiscoveryGuild(guildId: string): Promise<boolean> {
	try {
		await DiscoveryCommands.joinGuild(guildId);
		NavigationCommands.selectGuild(guildId);
		return true;
	} catch (error) {
		showJoinGuildErrorModal(error);
		return false;
	}
}
