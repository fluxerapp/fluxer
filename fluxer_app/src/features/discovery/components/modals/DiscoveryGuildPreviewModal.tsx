// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {PreviewGuildInviteHeader} from '@app/features/auth/flow/InviteHeader';
import type {DiscoveryGuild} from '@app/features/discovery/commands/DiscoveryCommands';
import {joinDiscoveryGuild} from '@app/features/discovery/commands/DiscoveryJoinCommands';
import styles from '@app/features/discovery/components/modals/DiscoveryGuildPreviewModal.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import inviteStyles from '@app/features/invite/components/modals/InviteAcceptModal.module.css';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

const COMMUNITY_PREVIEW_DESCRIPTOR = msg({
	message: 'Community preview',
	comment: 'Accessible title of the modal that previews a Discovery community before joining it.',
});
const OPEN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Open community',
	comment: 'Button label in the Discovery community preview modal shown when the user is already a member.',
});
const NO_DESCRIPTION_DESCRIPTOR = msg({
	message: 'No description.',
	comment: 'Empty-state text shown in the Discovery community preview modal when a community has no description.',
});
const PUBLIC_COMMUNITY_DESCRIPTOR = msg({
	message: 'Public community',
	comment:
		'Line shown above the community name in the Discovery community preview modal. The user found this community in the public directory rather than being invited to it.',
});

interface DiscoveryGuildPreviewModalProps {
	guild: DiscoveryGuild;
}

export const DiscoveryGuildPreviewModal = observer(function DiscoveryGuildPreviewModal({
	guild,
}: DiscoveryGuildPreviewModalProps) {
	const {i18n} = useLingui();
	const [joining, setJoining] = useState(false);
	const isAlreadyMember = Guilds.getGuild(guild.id) != null;
	const badgeFeatures = useMemo(
		() => guild.features.filter((feature) => feature !== GuildFeatures.DISCOVERABLE),
		[guild.features],
	);
	const handleDismiss = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleJoin = useCallback(async () => {
		if (joining) return;
		if (isAlreadyMember) {
			ModalCommands.pop();
			NavigationCommands.selectGuild(guild.id);
			return;
		}
		setJoining(true);
		const joined = await joinDiscoveryGuild(guild.id);
		if (joined) {
			ModalCommands.pop();
			return;
		}
		setJoining(false);
	}, [guild.id, isAlreadyMember, joining]);
	const canJoin = !RuntimeConfig.singleCommunityEnabled;
	return (
		<Modal.Root
			size="large"
			className={inviteStyles.root}
			centered
			onClose={handleDismiss}
			data-flx="discovery.discovery-guild-preview-modal.root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(COMMUNITY_PREVIEW_DESCRIPTOR)}
				data-flx="discovery.discovery-guild-preview-modal.modal-screen-reader-label"
			/>
			<Modal.InsetCloseButton
				onClick={handleDismiss}
				disabled={joining}
				data-flx="discovery.discovery-guild-preview-modal.modal-inset-close-button.dismiss"
			/>
			<div
				className={inviteStyles.background}
				aria-hidden
				data-flx="discovery.discovery-guild-preview-modal.background"
			>
				<div
					className={inviteStyles.patternImage}
					style={{backgroundImage: `url(${foodPatternUrl})`}}
					data-flx="discovery.discovery-guild-preview-modal.pattern-image"
				/>
			</div>
			<div className={inviteStyles.cardHost} data-flx="discovery.discovery-guild-preview-modal.card-host">
				<div className={inviteStyles.card} data-flx="discovery.discovery-guild-preview-modal.card">
					<div className={inviteStyles.cardInner} data-flx="discovery.discovery-guild-preview-modal.card-inner">
						<PreviewGuildInviteHeader
							guildId={guild.id}
							guildName={guild.name}
							guildIcon={guild.icon}
							features={badgeFeatures}
							presenceCount={guild.online_count}
							memberCount={guild.member_count}
							eyebrowText={i18n._(PUBLIC_COMMUNITY_DESCRIPTOR)}
							data-flx="discovery.discovery-guild-preview-modal.preview-guild-invite-header"
						/>
						<p className={styles.description} data-flx="discovery.discovery-guild-preview-modal.description">
							{guild.description || i18n._(NO_DESCRIPTION_DESCRIPTOR)}
						</p>
						{(isAlreadyMember || canJoin) && (
							<div className={inviteStyles.actions} data-flx="discovery.discovery-guild-preview-modal.actions">
								<Button
									onClick={handleJoin}
									disabled={joining}
									submitting={joining}
									data-flx="discovery.discovery-guild-preview-modal.button.join"
								>
									{isAlreadyMember ? i18n._(OPEN_COMMUNITY_DESCRIPTOR) : i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
								</Button>
							</div>
						)}
					</div>
				</div>
			</div>
		</Modal.Root>
	);
});
