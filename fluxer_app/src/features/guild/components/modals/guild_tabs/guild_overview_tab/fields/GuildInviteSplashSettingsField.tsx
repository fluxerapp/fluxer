// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTab.module.css';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {CardAlignmentControls} from '@app/features/ui/card_alignment_controls/CardAlignmentControls';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EyeIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';

const ALIGNMENT_CONTROLS_ARE_ONLY_AVAILABLE_ON_WIDER_SCREENS_DESCRIPTOR = msg({
	message: 'Alignment controls are only available on wider screens',
	comment: 'Label in the guild invite splash settings field.',
});
export const GuildInviteSplashSettingsField: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	onPreviewInvitePage?: () => void;
	onPreviewInviteModal?: () => void;
}> = ({form, canManageGuild, onPreviewInvitePage, onPreviewInviteModal}) => {
	const {i18n} = useLingui();
	const alignment = form.watch('splash_card_alignment', GuildSplashCardAlignment.CENTER);
	return (
		<div
			className={styles.splashSettingsContainer}
			data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-container"
		>
			<div
				className={styles.splashSettingsRow}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-row"
			>
				<div
					className={styles.splashSettingsColumn}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-column"
				>
					<div
						className={styles.iconField}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.icon-field"
					>
						<Trans>Preview</Trans>
					</div>
					<div
						className={styles.splashSettingsButtons}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-buttons"
					>
						{onPreviewInvitePage ? (
							<Button
								variant="secondary"
								small={true}
								onClick={onPreviewInvitePage}
								disabled={!canManageGuild}
								className={styles.invitePageButton}
								data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.invite-page-button.preview-invite-page"
							>
								<EyeIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.eye-icon"
								/>
								<Trans>Invite page</Trans>
							</Button>
						) : null}
						{onPreviewInviteModal ? (
							<Button
								variant="secondary"
								small={true}
								onClick={onPreviewInviteModal}
								disabled={!canManageGuild}
								data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.button.preview-invite-modal"
							>
								<EyeIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.eye-icon--2"
								/>
								<Trans>Invite modal</Trans>
							</Button>
						) : null}
					</div>
					<p
						className={styles.splashSettingsHelper}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-helper"
					>
						<Trans>See how your invite looks to visitors.</Trans>
					</p>
				</div>
				<div
					className={clsx(styles.splashSettingsColumn, styles.splashSettingsColumnRight)}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-column--2"
				>
					<div
						className={styles.iconField}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.icon-field--2"
					>
						<Trans>Card alignment</Trans>
					</div>
					<div
						className={styles.alignmentControlsRow}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.alignment-controls-row"
					>
						<CardAlignmentControls
							value={alignment}
							onChange={(value) => form.setValue('splash_card_alignment', value, {shouldDirty: true})}
							disabled={!canManageGuild}
							className={styles.cardAlignmentControls}
							disabledTooltipText={i18n._(ALIGNMENT_CONTROLS_ARE_ONLY_AVAILABLE_ON_WIDER_SCREENS_DESCRIPTOR)}
							data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.card-alignment-controls.set-value"
						/>
					</div>
					<p
						className={styles.splashSettingsHelper}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-settings-field.splash-settings-helper--2"
					>
						<Trans>Only applies on wide screens.</Trans>
					</p>
				</div>
			</div>
		</div>
	);
};
