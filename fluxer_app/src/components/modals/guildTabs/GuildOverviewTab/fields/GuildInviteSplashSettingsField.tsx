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
import {EyeIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';
import {GuildSplashCardAlignment} from '~/Constants';
import {Button} from '~/components/uikit/Button/Button';
import {CardAlignmentControls} from '~/components/uikit/CardAlignmentControls/CardAlignmentControls';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import styles from '../GuildOverviewTab.module.css';

export const GuildInviteSplashSettingsField: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	onPreviewInvitePage?: () => void;
	onPreviewInviteModal?: () => void;
}> = ({form, canManageGuild, onPreviewInvitePage, onPreviewInviteModal}) => {
	const {t} = useLingui();
	const alignment = form.watch('splash_card_alignment', GuildSplashCardAlignment.CENTER);

	return (
		<div className={styles.splashSettingsContainer}>
			<div className={styles.splashSettingsRow}>
				<div className={styles.splashSettingsColumn}>
					<div className={styles.iconField}>
						<Trans>Preview</Trans>
					</div>
					<div className={styles.splashSettingsButtons}>
						{onPreviewInvitePage ? (
							<Button
								variant="secondary"
								small={true}
								onClick={onPreviewInvitePage}
								disabled={!canManageGuild}
								className={styles.invitePageButton}
							>
								<EyeIcon size={16} weight="bold" />
								<Trans>Invite Page</Trans>
							</Button>
						) : null}
						{onPreviewInviteModal ? (
							<Button variant="secondary" small={true} onClick={onPreviewInviteModal} disabled={!canManageGuild}>
								<EyeIcon size={16} weight="bold" />
								<Trans>Invite Modal</Trans>
							</Button>
						) : null}
					</div>
					<p className={styles.splashSettingsHelper}>
						<Trans>See how your invite looks to visitors.</Trans>
					</p>
				</div>

				<div className={clsx(styles.splashSettingsColumn, styles.splashSettingsColumnRight)}>
					<div className={styles.iconField}>
						<Trans>Card Alignment</Trans>
					</div>
					<div className={styles.alignmentControlsRow}>
						<CardAlignmentControls
							value={alignment}
							onChange={(value) => form.setValue('splash_card_alignment', value, {shouldDirty: true})}
							disabled={!canManageGuild}
							className={styles.cardAlignmentControls}
							disabledTooltipText={t`Alignment controls are only available on wider screens`}
						/>
					</div>
					<p className={styles.splashSettingsHelper}>
						<Trans>Only applies on wide screens.</Trans>
					</p>
				</div>
			</div>
		</div>
	);
};
