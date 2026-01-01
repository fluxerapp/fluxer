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
import {DownloadSimpleIcon, ImageSquareIcon, StarIcon, TrashIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {ExpiryFootnote} from '~/components/common/ExpiryFootnote';
import {Switch} from '~/components/form/Switch';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {SwitchGroup, SwitchGroupItem} from '~/components/uikit/SwitchGroup';
import AccessibilityStore, {MediaDimensionSize} from '~/stores/AccessibilityStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './MediaTab.module.css';

const MediaPreview = observer(
	({
		showFavorite,
		showDownload,
		showDelete,
		showSuppress,
		showGifIndicator,
		showExpiryIndicator,
	}: {
		showFavorite: boolean;
		showDownload: boolean;
		showDelete: boolean;
		showSuppress: boolean;
		showGifIndicator: boolean;
		showExpiryIndicator: boolean;
	}) => {
		const previewExpiry = React.useMemo(() => new Date(Date.now() + 12 * 24 * 60 * 60 * 1000), []);
		return (
			<div className={styles.previewContainer}>
				<div className={styles.previewWrapper}>
					<div className={styles.previewBox}>
						<ImageSquareIcon className={styles.previewIcon} />
						{showGifIndicator && <div className={styles.gifIndicator}>GIF</div>}
						<div className={styles.actionButtons}>
							{showDelete && (
								<button type="button" className={styles.actionButton}>
									<TrashIcon size={18} weight="bold" className={styles.actionButtonIcon} />
								</button>
							)}
							{showDownload && (
								<button type="button" className={styles.actionButton}>
									<DownloadSimpleIcon size={18} weight="bold" className={styles.actionButtonIcon} />
								</button>
							)}
							{showFavorite && (
								<button type="button" className={styles.actionButton}>
									<StarIcon size={18} weight="bold" className={styles.actionButtonIcon} />
								</button>
							)}
						</div>
					</div>
					{showSuppress && (
						<button type="button" className={styles.suppressButton}>
							<XIcon size={16} weight="bold" />
						</button>
					)}
					{showExpiryIndicator && (
						<ExpiryFootnote expiresAt={previewExpiry} isExpired={false} className={styles.expiryFootnotePreview} />
					)}
				</div>
			</div>
		);
	},
);

export const MediaTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const mobileLayout = MobileLayoutStore;

	const {
		showMediaFavoriteButton,
		showMediaDownloadButton,
		showMediaDeleteButton,
		showSuppressEmbedsButton,
		showGifIndicator,
		showAttachmentExpiryIndicator,
		autoSendTenorGifs,
		embedMediaDimensionSize,
		attachmentMediaDimensionSize,
	} = AccessibilityStore;

	const mediaSizeOptions = React.useMemo(
		(): ReadonlyArray<RadioOption<MediaDimensionSize>> => [
			{
				value: MediaDimensionSize.SMALL,
				name: t`Compact (400x300)`,
				desc: t`Smaller media size`,
			},
			{
				value: MediaDimensionSize.LARGE,
				name: t`Comfortable (550x400)`,
				desc: t`Larger media size with more detail`,
			},
		],
		[t],
	);

	return (
		<>
			<SettingsTabSection
				title="Media Size Preferences"
				description="Customize the maximum display size for embedded and attached media. Smaller sizes use less screen space, while larger sizes show more detail."
			>
				<div className={styles.radioSections}>
					<div className={styles.radioSection}>
						<div className={styles.radioLabelContainer}>
							<div className={styles.radioLabel}>
								<Trans>Media from links (embeds)</Trans>
							</div>
						</div>
						<RadioGroup
							options={mediaSizeOptions}
							value={embedMediaDimensionSize}
							onChange={(value) => AccessibilityActionCreators.update({embedMediaDimensionSize: value})}
							aria-label={t`Select media size for embedded content from links`}
						/>
					</div>

					<div className={styles.radioSection}>
						<div className={styles.radioLabelContainer}>
							<div className={styles.radioLabel}>
								<Trans>Uploaded attachments</Trans>
							</div>
						</div>
						<RadioGroup
							options={mediaSizeOptions}
							value={attachmentMediaDimensionSize}
							onChange={(value) => AccessibilityActionCreators.update({attachmentMediaDimensionSize: value})}
							aria-label={t`Select media size for uploaded attachments`}
						/>
					</div>
				</div>
			</SettingsTabSection>

			<SettingsTabSection title="GIF Behavior" description="Control how GIFs are inserted into chat">
				<div className={styles.sectionContent}>
					<Switch
						label={t`Automatically send Tenor GIFs when selected`}
						value={autoSendTenorGifs}
						onChange={(value) => AccessibilityActionCreators.update({autoSendTenorGifs: value})}
					/>
				</div>
			</SettingsTabSection>

			{!mobileLayout.enabled && (
				<SettingsTabSection
					title="Media Buttons"
					description="Customize which buttons appear on media attachments and embeds when hovering over messages."
				>
					<div className={styles.sectionContent}>
						<MediaPreview
							showFavorite={showMediaFavoriteButton}
							showDownload={showMediaDownloadButton}
							showDelete={showMediaDeleteButton}
							showSuppress={showSuppressEmbedsButton}
							showGifIndicator={showGifIndicator}
							showExpiryIndicator={showAttachmentExpiryIndicator}
						/>
						<SwitchGroup>
							<SwitchGroupItem
								label={t`Show GIF Indicator`}
								value={showGifIndicator}
								onChange={(value) => AccessibilityActionCreators.update({showGifIndicator: value})}
							/>
							<SwitchGroupItem
								label={t`Show Attachment Expiry Indicator`}
								value={showAttachmentExpiryIndicator}
								onChange={(value) => AccessibilityActionCreators.update({showAttachmentExpiryIndicator: value})}
							/>
							<SwitchGroupItem
								label={t`Show Delete Button`}
								value={showMediaDeleteButton}
								onChange={(value) => AccessibilityActionCreators.update({showMediaDeleteButton: value})}
							/>
							<SwitchGroupItem
								label={t`Show Download Button`}
								value={showMediaDownloadButton}
								onChange={(value) => AccessibilityActionCreators.update({showMediaDownloadButton: value})}
							/>
							<SwitchGroupItem
								label={t`Show Favorite Button`}
								value={showMediaFavoriteButton}
								onChange={(value) => AccessibilityActionCreators.update({showMediaFavoriteButton: value})}
							/>
							<SwitchGroupItem
								label={t`Show Suppress Embeds Button`}
								value={showSuppressEmbedsButton}
								onChange={(value) => AccessibilityActionCreators.update({showSuppressEmbedsButton: value})}
							/>
						</SwitchGroup>
					</div>
				</SettingsTabSection>
			)}
		</>
	);
});
