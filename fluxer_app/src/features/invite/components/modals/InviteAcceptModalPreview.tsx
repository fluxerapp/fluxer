// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PreviewGuildInviteHeader} from '@app/features/auth/flow/InviteHeader';
import type {Guild} from '@app/features/guild/models/Guild';
import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/invite/components/modals/InviteAcceptModal.module.css';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Presence from '@app/features/presence/state/Presence';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const INVITE_MODAL_PREVIEW_DESCRIPTOR = msg({
	message: 'Invite modal preview',
	comment: 'Button or menu action label in the invite accept modal preview. Keep it concise.',
});

interface InviteAcceptModalPreviewProps {
	guild: Guild;
	previewName: string | null | undefined;
	previewIconUrl: string | null;
	hasClearedIcon: boolean;
	previewSplashUrl: string | null;
	hasClearedSplash: boolean;
}

export const InviteAcceptModalPreview = observer(function InviteAcceptModalPreview({
	guild,
	previewName,
	previewIconUrl,
	hasClearedIcon,
	previewSplashUrl,
	hasClearedSplash,
}: InviteAcceptModalPreviewProps) {
	const {i18n} = useLingui();
	const handleDismiss = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const presenceCount = Presence.getPresenceCount(guild.id);
	const memberCount = GuildMembers.getMemberCount(guild.id);
	const guildFeatures = useMemo(() => {
		return Array.from(guild.features);
	}, [guild.features]);
	const splashUrl = useMemo(() => {
		if (hasClearedSplash) {
			return null;
		}
		if (previewSplashUrl) {
			return previewSplashUrl;
		}
		if (guild.splash) {
			return AvatarUtils.getGuildSplashURL({
				id: guild.id,
				splash: guild.splash,
			});
		}
		return null;
	}, [guild.id, guild.splash, hasClearedSplash, previewSplashUrl]);
	return (
		<Modal.Root
			size="large"
			className={styles.root}
			centered
			onClose={handleDismiss}
			data-flx="invite.invite-accept-modal-preview.root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(INVITE_MODAL_PREVIEW_DESCRIPTOR)}
				data-flx="invite.invite-accept-modal-preview.modal-screen-reader-label"
			/>
			<Modal.InsetCloseButton
				onClick={handleDismiss}
				disabled={false}
				data-flx="invite.invite-accept-modal-preview.modal-inset-close-button.dismiss"
			/>
			<div className={styles.background} aria-hidden data-flx="invite.invite-accept-modal-preview.background">
				{splashUrl ? (
					<div
						className={styles.splashImage}
						style={{backgroundImage: `url(${splashUrl})`}}
						data-flx="invite.invite-accept-modal-preview.splash-image"
					/>
				) : (
					<div
						className={styles.patternImage}
						style={{backgroundImage: `url(${foodPatternUrl})`}}
						data-flx="invite.invite-accept-modal-preview.pattern-image"
					/>
				)}
			</div>
			<div className={styles.cardHost} data-flx="invite.invite-accept-modal-preview.card-host">
				<div className={styles.card} data-flx="invite.invite-accept-modal-preview.card">
					<div className={styles.cardInner} data-flx="invite.invite-accept-modal-preview.card-inner">
						<PreviewGuildInviteHeader
							guildId={guild.id}
							guildName={guild.name}
							guildIcon={guild.icon}
							features={guildFeatures}
							badgeIds={guild.badgeIds}
							presenceCount={presenceCount}
							memberCount={memberCount}
							previewIconUrl={hasClearedIcon ? null : previewIconUrl}
							previewName={previewName}
							data-flx="invite.invite-accept-modal-preview.preview-guild-invite-header"
						/>
						<div className={styles.actions} data-flx="invite.invite-accept-modal-preview.actions">
							<Button disabled={true} data-flx="invite.invite-accept-modal-preview.button">
								{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
							</Button>
						</div>
					</div>
				</div>
			</div>
		</Modal.Root>
	);
});
