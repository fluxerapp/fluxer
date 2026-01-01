/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import type {GuildSplashCardAlignmentValue} from '~/Constants';
import {GuildFeatures, GuildSplashCardAlignment} from '~/Constants';
import {AuthBackground} from '~/components/auth/AuthBackground';
import {AuthBottomLink} from '~/components/auth/AuthBottomLink';
import {AuthCardContainer} from '~/components/auth/AuthCardContainer';
import authPageStyles from '~/components/auth/AuthPageStyles.module.css';
import {PreviewGuildInviteHeader} from '~/components/auth/InviteHeader';
import {MockMinimalRegisterForm} from '~/components/auth/MockMinimalRegisterForm';
import authLayoutStyles from '~/components/layout/AuthLayout.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {CardAlignmentControls} from '~/components/uikit/CardAlignmentControls/CardAlignmentControls';
import {useAuthBackground} from '~/hooks/useAuthBackground';
import foodPatternUrl from '~/images/i-like-food.svg';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import PresenceStore from '~/stores/PresenceStore';
import WindowStore from '~/stores/WindowStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './InvitePagePreviewModal.module.css';
import * as Modal from './Modal';

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
		const {t} = useLingui();
		const guild = GuildStore.getGuild(guildId);
		const initialAlignment = previewSplashAlignment ?? guild?.splashCardAlignment ?? GuildSplashCardAlignment.CENTER;
		const [localAlignment, setLocalAlignment] = React.useState<GuildSplashCardAlignmentValue>(initialAlignment);
		const alignmentControlsEnabled = WindowStore.windowSize.width >= ALIGNMENT_MIN_WIDTH;

		const splashUrl = React.useMemo(() => {
			if (previewSplashUrl) return previewSplashUrl;
			if (guild?.splash) {
				return AvatarUtils.getGuildSplashURL({id: guild.id, splash: guild.splash}, 4096);
			}
			return null;
		}, [previewSplashUrl, guild?.id, guild?.splash]);

		const {patternReady, splashLoaded, splashDimensions} = useAuthBackground(splashUrl, foodPatternUrl);
		const shouldShowSplash = Boolean(splashUrl && splashDimensions);

		const handleClose = React.useCallback(() => {
			ModalActionCreators.pop();
		}, []);

		const handleAlignmentChange = React.useCallback(
			(alignment: GuildSplashCardAlignmentValue) => {
				setLocalAlignment(alignment);
				onAlignmentChange?.(alignment);
			},
			[onAlignmentChange],
		);

		if (!guild) return null;

		const splashAlignment = localAlignment;

		const isVerified = guild.features.has(GuildFeatures.VERIFIED);
		const presenceCount = PresenceStore.getPresenceCount(guildId);
		const memberCount = GuildMemberStore.getMemberCount(guildId);

		return (
			<Modal.Root size="fullscreen" className={styles.previewModal} onClose={handleClose}>
				<Modal.ScreenReaderLabel text={t`Invite Page Preview`} />

				<div className={styles.previewPillContainer}>
					<div className={styles.previewPill}>
						<span className={styles.previewPillText}>
							<Trans>You're in preview mode</Trans>
						</span>
						<Button small variant="primary" onClick={handleClose} className={styles.exitButton}>
							<Trans>Exit Preview</Trans>
						</Button>
					</div>
				</div>

				<div className={styles.alignmentControlsContainer}>
					<CardAlignmentControls
						value={localAlignment}
						onChange={handleAlignmentChange}
						disabled={!alignmentControlsEnabled}
						disabledTooltipText={t`Alignment controls are only available on wider screens`}
						tooltipPosition="top"
					/>
				</div>

				<div className={styles.previewContent}>
					<AuthBackground
						className={clsx(styles.background, !shouldShowSplash && authLayoutStyles.patternHost)}
						splashUrl={splashUrl}
						splashLoaded={splashLoaded}
						splashDimensions={splashDimensions}
						patternReady={patternReady}
						patternImageUrl={foodPatternUrl}
						splashAlignment={splashAlignment}
						useFullCover={true}
					/>

					<div className={styles.foreground}>
						<div
							className={clsx(
								authLayoutStyles.leftSplit,
								splashAlignment === GuildSplashCardAlignment.LEFT && authLayoutStyles.alignLeft,
								splashAlignment === GuildSplashCardAlignment.RIGHT && authLayoutStyles.alignRight,
							)}
						>
							<div className={authLayoutStyles.leftSplitWrapper}>
								<div className={authLayoutStyles.leftSplitAnimated}>
									<AuthCardContainer
										showLogoSide={true}
										isInert={true}
										className={clsx(authLayoutStyles.cardContainer, styles.cardContainer)}
									>
										<PreviewGuildInviteHeader
											guildId={guild.id}
											guildName={guild.name}
											guildIcon={guild.icon}
											isVerified={isVerified}
											presenceCount={presenceCount}
											memberCount={memberCount}
											previewIconUrl={previewIconUrl}
											previewName={previewName}
										/>

										<div className={authPageStyles.container}>
											<MockMinimalRegisterForm submitLabel={<Trans>Create account</Trans>} />
											<AuthBottomLink variant="login" to="/login" />
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
