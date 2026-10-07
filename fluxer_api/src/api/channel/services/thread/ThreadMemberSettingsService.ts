// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {dispatchThreadEvents, threadMemberUpdateEvent} from '@app/api/channel/services/thread/ThreadDispatch';
import {mapThreadMemberToResponse} from '@app/api/channel/services/thread/ThreadMappers';
import type {ThreadServiceContext} from '@app/api/channel/services/thread/ThreadServiceContext';
import type {MuteConfig} from '@app/api/database/types/UserTypes';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import {isValidThreadMemberSettingsFlags, ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {InvalidThreadNotificationSettingsError} from '@fluxer/errors/src/domains/channel/InvalidThreadNotificationSettingsError';
import {UnknownThreadMemberError} from '@fluxer/errors/src/domains/channel/UnknownThreadMemberError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import type {ThreadMemberSettingsRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';

const SETTINGS_CAS_ATTEMPTS = 3;

function sameMuteConfig(a: MuteConfig | null, b: MuteConfig | null): boolean {
	if (a === null || b === null) return a === b;
	return (
		(a.end_time?.getTime() ?? null) === (b.end_time?.getTime() ?? null) &&
		a.selected_time_window === b.selected_time_window
	);
}

export class ThreadMemberSettingsService {
	constructor(private readonly ctx: ThreadServiceContext) {}

	async update(params: {
		viewer: ThreadViewer;
		userId: UserID;
		channelId: ChannelID;
		data: ThreadMemberSettingsRequest;
	}): Promise<ThreadMemberResponse | null> {
		const {data, userId} = params;
		if (data.flags !== undefined && !isValidThreadMemberSettingsFlags(data.flags)) {
			throw new InvalidThreadNotificationSettingsError();
		}
		const auth = await this.ctx.channelAuth.getChannelAuthenticated({
			userId,
			channelId: params.channelId,
			viewer: params.viewer,
			skipNsfwValidation: true,
		});
		if (!auth.thread) throw new InvalidChannelTypeError();
		const threadId = auth.thread.state.threadId;
		for (let attempt = 0; attempt < SETTINGS_CAS_ATTEMPTS; attempt++) {
			const member =
				(attempt === 0 ? auth.thread.member : null) ??
				(await this.ctx.channelRepository.threads.getMember(threadId, userId));
			if (!member) throw new UnknownThreadMemberError();
			const flags =
				data.flags === undefined ? member.flags : (member.flags & ThreadMemberFlags.HAS_INTERACTED) | data.flags;
			const muted = data.muted ?? member.muted;
			const current = member.muteConfig?.toMuteConfig() ?? null;
			const muteConfig =
				data.mute_config === undefined
					? current
					: data.mute_config === null
						? null
						: {
								end_time: data.mute_config.end_time ?? null,
								selected_time_window: data.mute_config.selected_time_window,
							};
			if (flags === member.flags && muted === member.muted && sameMuteConfig(muteConfig, current)) return null;
			const updated = await this.ctx.channelRepository.threads.updateMemberSettings(member, {
				flags,
				muted,
				...(data.mute_config === undefined ? {} : {muteConfig}),
			});
			if (!updated) continue;
			await this.dispatch(updated);
			return mapThreadMemberToResponse(updated, {self: true});
		}
		throw new ServiceUnavailableError();
	}

	private async dispatch(member: ThreadMember): Promise<void> {
		await dispatchThreadEvents(this.ctx.gatewayService, member.guildId, [threadMemberUpdateEvent(member)]);
	}
}
