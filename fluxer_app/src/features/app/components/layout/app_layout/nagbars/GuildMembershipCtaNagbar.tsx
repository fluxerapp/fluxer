// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_HQ_COMMUNITY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import Guilds from '@app/features/guild/state/Guilds';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {InviteAcceptModal} from '@app/features/invite/components/modals/InviteAcceptModal';
import Invites from '@app/features/invite/state/Invites';
import {isGuildInvite} from '@app/features/invite/types/InviteTypes';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import NagbarState from '@app/features/ui/state/Nagbar';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const FLUXER_HQ_INVITE_CODE = 'fluxer-hq';
const JOIN_PRODUCT_COMMUNITY_MESSAGE_DESCRIPTOR = msg({
	message: 'Join {communityName} to chat with the team and stay up to date.',
	comment: 'Nagbar body inviting users to join the official product community. communityName is the community name.',
});
const JOIN_PRODUCT_COMMUNITY_BUTTON_DESCRIPTOR = msg({
	message: 'Join {communityName}',
	comment: 'Nagbar button label that opens the official product community invite. communityName is the community name.',
});
export const GuildMembershipCtaNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const isSelfHosted = RuntimeConfig.isSelfHosted();
	const currentUserId = Authentication.currentUserId;
	const inviteState = Invites.invites.get(FLUXER_HQ_INVITE_CODE);
	const invite = inviteState?.data ?? null;
	const [isSubmitting, setIsSubmitting] = useState(false);
	useEffect(() => {
		const fluxerHqGuild = Guilds.getGuilds().find((guild) => guild.vanityURLCode === FLUXER_HQ_INVITE_CODE);
		if (fluxerHqGuild && GuildMembers.getMember(fluxerHqGuild.id, currentUserId ?? '')) {
			NagbarState.guildMembershipCtaDismissed = true;
		}
	}, [currentUserId]);
	if (isSelfHosted) {
		return null;
	}
	if (!currentUserId) {
		return null;
	}
	if (invite && isGuildInvite(invite)) {
		const guildId = invite.guild.id;
		const isMember = Boolean(GuildMembers.getMember(guildId, currentUserId));
		if (isMember) {
			return null;
		}
	}
	const handleJoinGuild = async () => {
		if (isSubmitting) return;
		setIsSubmitting(true);
		try {
			await InviteCommands.fetchWithCoalescing(FLUXER_HQ_INVITE_CODE);
		} finally {
			setIsSubmitting(false);
			ModalCommands.push(
				modal(() => (
					<InviteAcceptModal
						code={FLUXER_HQ_INVITE_CODE}
						data-flx="app.app-layout.nagbars.guild-membership-cta-nagbar.handle-join-guild.invite-accept-modal"
					/>
				)),
			);
		}
	};
	const handleDismiss = () => {
		NagbarState.guildMembershipCtaDismissed = true;
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			onDismiss={handleDismiss}
			dismissible={true}
			data-flx="app.app-layout.nagbars.guild-membership-cta-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(JOIN_PRODUCT_COMMUNITY_MESSAGE_DESCRIPTOR, {
					communityName: PRODUCT_HQ_COMMUNITY_NAME,
				})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleJoinGuild}
						submitting={isSubmitting}
						disabled={isSubmitting}
						data-flx="app.app-layout.nagbars.guild-membership-cta-nagbar.nagbar-button.join-guild"
					>
						{i18n._(JOIN_PRODUCT_COMMUNITY_BUTTON_DESCRIPTOR, {communityName: PRODUCT_HQ_COMMUNITY_NAME})}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.guild-membership-cta-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
