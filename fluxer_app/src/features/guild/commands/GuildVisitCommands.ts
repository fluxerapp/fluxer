// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {promptForAccountTier} from '@app/features/auth/utils/AccountTierPrompt';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVisits from '@app/features/guild/state/GuildVisits';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {GuildVisitStartResponse} from '@fluxer/schema/src/domains/guild/GuildVisitSchemas';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {when} from 'mobx';

const VISIT_ENDED_DESCRIPTOR = msg({
	message: "You're no longer previewing {community}.",
	comment:
		'Quiet toast when the preview of a community ends without the person ending it. community is its name. Previewing means reading before joining.',
});

const ADULTS_ONLY_COMMUNITY_DESCRIPTOR = msg({
	message: 'This community is for adults only.',
	comment: 'Toast when someone who is not an adult tries to join a community marked 18+.',
});
const JOIN_FAILED_DESCRIPTOR = msg({
	message: "Couldn't join this community. Try again in a moment.",
	comment: 'Toast when joining a community from its preview fails for an unexpected reason.',
});

const STOP_PREVIEWING_FAILED_DESCRIPTOR = msg({
	message: "Couldn't stop previewing. Try again in a moment.",
	comment:
		'Toast when ending the preview of a community fails. Previewing means reading a community before joining it.',
});

const GUILD_ARRIVAL_TIMEOUT_MS = 15_000;

export async function openGuildWhenAvailable(guildId: string, channelId?: string): Promise<void> {
	const arrived = await when(() => Guilds.getGuild(guildId) != null, {timeout: GUILD_ARRIVAL_TIMEOUT_MS}).then(
		() => true,
		() => false,
	);
	if (arrived) NavigationCommands.selectChannel(guildId, channelId);
}

const discoveryVisitsInFlight = new Set<string>();

export async function visitFromDiscovery(guildId: string, fallback: () => void): Promise<void> {
	if (discoveryVisitsInFlight.has(guildId)) return;
	discoveryVisitsInFlight.add(guildId);
	try {
		const response = await http.post<GuildVisitStartResponse>(Endpoints.DISCOVERY_VISIT(guildId), {body: {}});
		await openGuildWhenAvailable(response.body.guild_id);
	} catch (error) {
		if (failureCode(error) === APIErrorCodes.GUILD_VISIT_AGE_RESTRICTED) {
			ToastCommands.error(i18n._(ADULTS_ONLY_COMMUNITY_DESCRIPTOR));
			return;
		}
		fallback();
	} finally {
		discoveryVisitsInFlight.delete(guildId);
	}
}

export async function joinVisitedGuild(guildId: string): Promise<void> {
	try {
		await http.post(Endpoints.USER_GUILD_VISIT_JOIN(guildId), {body: {}});
		await when(() => Guilds.getGuild(guildId)?.visiting !== true, {timeout: GUILD_ARRIVAL_TIMEOUT_MS}).catch(() => {});
	} catch (error) {
		if (promptForAccountTier(error, () => void joinVisitedGuild(guildId))) {
			return;
		}
		const code = failureCode(error);
		if (code === APIErrorCodes.GUILD_VISIT_NOT_FOUND || code === APIErrorCodes.UNKNOWN_INVITE) {
			showVisitEndedToast(Guilds.getGuild(guildId)?.name ?? '');
			return;
		}
		if (code === APIErrorCodes.GUILD_VISIT_AGE_RESTRICTED) {
			ToastCommands.error(i18n._(ADULTS_ONLY_COMMUNITY_DESCRIPTOR));
			return;
		}
		ToastCommands.error(failureMessage(error) ?? i18n._(JOIN_FAILED_DESCRIPTOR));
	}
}

export function showVisitEndedToast(community: string): void {
	ToastCommands.createToast({
		type: 'info',
		children: i18n._(VISIT_ENDED_DESCRIPTOR, {community: isolateBidi(community)}),
	});
}

export async function leaveVisit(guildId: string): Promise<void> {
	GuildVisits.markEndedByUser(guildId);
	try {
		await http.delete(Endpoints.USER_GUILD_VISIT(guildId));
	} catch {
		GuildVisits.consumeEndedByUser(guildId);
		ToastCommands.error(i18n._(STOP_PREVIEWING_FAILED_DESCRIPTOR));
		return;
	}
	if (SelectedGuild.selectedGuildId === guildId) RouterUtils.transitionTo(Routes.ME);
}
