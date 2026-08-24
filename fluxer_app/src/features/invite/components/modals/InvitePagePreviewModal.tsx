// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import authLayoutStyles from '@app/features/app/components/layout/AuthLayout.module.css';
import {AuthBackground} from '@app/features/auth/flow/AuthBackground';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthCardContainer} from '@app/features/auth/flow/AuthCardContainer';
import authPageStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {PreviewGuildInviteHeader} from '@app/features/auth/flow/InviteHeader';
import {MockMinimalRegisterForm} from '@app/features/auth/flow/MockMinimalRegisterForm';
import {useAuthBackground} from '@app/features/auth/hooks/useAuthBackground';
import Guilds from '@app/features/guild/state/Guilds';
import styles from '@app/features/invite/components/modals/InvitePagePreviewModal.module.css';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Presence from '@app/features/presence/state/Presence';
import {Button} from '@app/features/ui/button/Button';
import {CardAlignmentControls} from '@app/features/ui/card_alignment_controls/CardAlignmentControls';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import Window from '@app/features/window/state/Window';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const INVITE_PAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'Invite page preview',
	comment: 'Button or menu action label in the invite page preview modal. Keep it concise.',
});
const ALIGNMENT_CONTROLS_ARE_ONLY_AVAILABLE_ON_WIDER_SCREENS_DESCRIPTOR = msg({
	message: 'Alignment controls are only available on wider screens',
	comment: 'Label in the invite page preview modal.',
});

interface InvitePagePreviewModalProps {
	guildId: string;
	previewSplashUrl?: string | null;
	previewIconUrl?: string | null;
	previewName?: string | null;
	previewSplashAlignment?: GuildSplashCardAlignmentValue;
	onAlignmentChange?: (alignment: GuildSplashCardAlignmentValue) => void;
}

const ALIGNMENT_MIN_WIDTH = 1600;
export const InvitePagePreviewModal: React.FC<InvitePagePreviewModalProps> = observer(
	({guildId, previewSplashUrl, previewIconUrl, previewName, previewSplashAlignment, onAlignmentChange}) => {
		const {i18n} = useLingui();
		const guild = Guilds.getGuild(guildId);
		const initialAlignment = previewSplashAlignment ?? guild?.splashCardAlignment ?? GuildSplashCardAlignment.CENTER;
		const [localAlignment, setLocalAlignment] = useState<GuildSplashCardAlignmentValue>(initialAlignment);
		const alignmentControlsEnabled = Window.windowSize.width >= ALIGNMENT_MIN_WIDTH;
		const splashUrl = useMemo(() => {
			if (previewSplashUrl) return previewSplashUrl;
			if (guild?.splash) {
				return AvatarUtils.getGuildSplashURL({id: guild.id, splash: guild.splash});
			}
			return null;
		}, [previewSplashUrl, guild?.id, guild?.splash]);
		const {patternReady, splashDimensions} = useAuthBackground(splashUrl, foodPatternUrl);
		const shouldShowSplash = Boolean(splashUrl && splashDimensions);
		const handleClose = useCallback(() => {
			ModalCommands.pop();
		}, []);
		const handleAlignmentChange = useCallback(
			(alignment: GuildSplashCardAlignmentValue) => {
				setLocalAlignment(alignment);
				onAlignmentChange?.(alignment);
			},
			[onAlignmentChange],
		);
		if (!guild) return null;
		const splashAlignment = localAlignment;
		const guildFeatures = Array.from(guild.features);
		const presenceCount = Presence.getPresenceCount(guildId);
		const memberCount = GuildMembers.getMemberCount(guildId);
		return (
			<Modal.Root
				size="fullscreen"
				className={styles.previewModal}
				onClose={handleClose}
				data-flx="invite.invite-page-preview-modal.preview-modal"
			>
				<Modal.ScreenReaderLabel
					text={i18n._(INVITE_PAGE_PREVIEW_DESCRIPTOR)}
					data-flx="invite.invite-page-preview-modal.modal-screen-reader-label"
				/>
				<div className={styles.previewPillContainer} data-flx="invite.invite-page-preview-modal.preview-pill-container">
					<div className={styles.previewPill} data-flx="invite.invite-page-preview-modal.preview-pill">
						<span className={styles.previewPillText} data-flx="invite.invite-page-preview-modal.preview-pill-text">
							<Trans>You're in preview mode</Trans>
						</span>
						<Button
							small
							variant="primary"
							onClick={handleClose}
							className={styles.exitButton}
							data-flx="invite.invite-page-preview-modal.exit-button.close"
						>
							<Trans>Exit preview</Trans>
						</Button>
					</div>
				</div>
				<div
					className={styles.alignmentControlsContainer}
					data-flx="invite.invite-page-preview-modal.alignment-controls-container"
				>
					<CardAlignmentControls
						value={localAlignment}
						onChange={handleAlignmentChange}
						disabled={!alignmentControlsEnabled}
						disabledTooltipText={i18n._(ALIGNMENT_CONTROLS_ARE_ONLY_AVAILABLE_ON_WIDER_SCREENS_DESCRIPTOR)}
						tooltipPosition="top"
						data-flx="invite.invite-page-preview-modal.card-alignment-controls.alignment-change"
					/>
				</div>
				<div className={styles.previewContent} data-flx="invite.invite-page-preview-modal.preview-content">
					<AuthBackground
						className={clsx(styles.background, !shouldShowSplash && authLayoutStyles.patternHost)}
						splashUrl={splashUrl}
						splashDimensions={splashDimensions}
						patternReady={patternReady}
						patternImageUrl={foodPatternUrl}
						splashAlignment={splashAlignment}
						useFullCover={true}
						data-flx="invite.invite-page-preview-modal.background"
					/>
					<div className={styles.foreground} data-flx="invite.invite-page-preview-modal.foreground">
						<div
							className={clsx(
								authLayoutStyles.leftSplit,
								splashAlignment === GuildSplashCardAlignment.LEFT && authLayoutStyles.alignLeft,
								splashAlignment === GuildSplashCardAlignment.RIGHT && authLayoutStyles.alignRight,
							)}
							data-flx="invite.invite-page-preview-modal.div"
						>
							<div className={authLayoutStyles.leftSplitWrapper} data-flx="invite.invite-page-preview-modal.div--2">
								<div className={authLayoutStyles.leftSplitAnimated} data-flx="invite.invite-page-preview-modal.div--3">
									<AuthCardContainer
										showLogoSide={true}
										isInert={true}
										className={clsx(authLayoutStyles.cardContainer, styles.cardContainer)}
										data-flx="invite.invite-page-preview-modal.card-container"
									>
										<PreviewGuildInviteHeader
											guildId={guild.id}
											guildName={guild.name}
											guildIcon={guild.icon}
											features={guildFeatures}
											presenceCount={presenceCount}
											memberCount={memberCount}
											previewIconUrl={previewIconUrl}
											previewName={previewName}
											data-flx="invite.invite-page-preview-modal.preview-guild-invite-header"
										/>
										<div className={authPageStyles.container} data-flx="invite.invite-page-preview-modal.div--4">
											<MockMinimalRegisterForm
												submitLabel={<Trans>Create account</Trans>}
												data-flx="invite.invite-page-preview-modal.mock-minimal-register-form"
											/>
											<AuthBottomLink
												variant="login"
												to="/login"
												data-flx="invite.invite-page-preview-modal.auth-bottom-link"
											/>
										</div>
									</AuthCardContainer>
								</div>
							</div>
						</div>
					</div>
				</div>
			</Modal.Root>
		);
	},
);
