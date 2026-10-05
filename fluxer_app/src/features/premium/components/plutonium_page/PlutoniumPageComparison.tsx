// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Limits} from '@app/features/app/utils/UserLimits';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPage.module.css';
import {
	PlutoniumPageIcon,
	type PlutoniumPageIconName,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageIcons';
import {
	AVAILABLE_DESCRIPTOR,
	COMPARE_TITLE_DESCRIPTOR,
	FEATURE_COLUMN_DESCRIPTOR,
	FREE_COLUMN_DESCRIPTOR,
	NOT_AVAILABLE_DESCRIPTOR,
	PERK_ANIMATED_EMOJIS_DESCRIPTOR,
	PERK_ANIMATED_PROFILE_DESCRIPTOR,
	PERK_BOOKMARKS_DESCRIPTOR,
	PERK_COMMUNITIES_DESCRIPTOR,
	PERK_CUSTOM_TAG_DESCRIPTOR,
	PERK_CUSTOM_THEMES_DESCRIPTOR,
	PERK_EARLY_ACCESS_DESCRIPTOR,
	PERK_GLOBAL_EXPRESSIONS_DESCRIPTOR,
	PERK_MESSAGE_CHARACTERS_DESCRIPTOR,
	PERK_PER_COMMUNITY_PROFILES_DESCRIPTOR,
	PERK_PROFILE_BADGE_DESCRIPTOR,
	PERK_SAVED_MEDIA_DESCRIPTOR,
	PERK_UPLOAD_SIZE_DESCRIPTOR,
	PERK_VIDEO_BACKGROUNDS_DESCRIPTOR,
	PERK_VIDEO_QUALITY_DESCRIPTOR,
	PERK_VIDEO_QUALITY_FREE_DESCRIPTOR,
	PERK_VIDEO_QUALITY_PREMIUM_DESCRIPTOR,
	TAG_FOOTNOTE_MARKER_DESCRIPTOR,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageMessages';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {
	isBooleanTierPerk,
	isNumericTierPerk,
	LIMIT_TIER_PERKS,
	type LimitTierPerk,
} from '@fluxer/constants/src/LimitTierPerks';
import type {MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

type PerkValue = {kind: 'boolean'; value: boolean} | {kind: 'text'; value: string};

interface PerkRow {
	id: string;
	icon: PlutoniumPageIconName;
	label: MessageDescriptor;
	free: PerkValue;
	premium: PerkValue;
}

interface PerkDefinition {
	perkId: string;
	icon: PlutoniumPageIconName;
	label: MessageDescriptor;
}

const PERK_DEFINITIONS: ReadonlyArray<PerkDefinition> = [
	{perkId: 'custom_discriminator', icon: 'hash', label: PERK_CUSTOM_TAG_DESCRIPTOR},
	{perkId: 'per_guild_profiles', icon: 'userCircle', label: PERK_PER_COMMUNITY_PROFILES_DESCRIPTOR},
	{perkId: 'profile_badge', icon: 'fluxerPremium', label: PERK_PROFILE_BADGE_DESCRIPTOR},
	{perkId: 'custom_video_backgrounds', icon: 'image', label: PERK_VIDEO_BACKGROUNDS_DESCRIPTOR},
	{perkId: 'max_guilds', icon: 'usersThree', label: PERK_COMMUNITIES_DESCRIPTOR},
	{perkId: 'max_message_length', icon: 'chatCenteredText', label: PERK_MESSAGE_CHARACTERS_DESCRIPTOR},
	{perkId: 'max_bookmarks', icon: 'bookmark', label: PERK_BOOKMARKS_DESCRIPTOR},
	{perkId: 'max_attachment_file_size', icon: 'paperclip', label: PERK_UPLOAD_SIZE_DESCRIPTOR},
	{perkId: 'max_favorite_memes', icon: 'images', label: PERK_SAVED_MEDIA_DESCRIPTOR},
	{perkId: 'use_animated_emojis', icon: 'smiley', label: PERK_ANIMATED_EMOJIS_DESCRIPTOR},
	{perkId: 'global_expressions', icon: 'globe', label: PERK_GLOBAL_EXPRESSIONS_DESCRIPTOR},
	{perkId: 'video_quality', icon: 'videoCamera', label: PERK_VIDEO_QUALITY_DESCRIPTOR},
	{perkId: 'animated_profile', icon: 'gif', label: PERK_ANIMATED_PROFILE_DESCRIPTOR},
	{perkId: 'early_access', icon: 'rocket', label: PERK_EARLY_ACCESS_DESCRIPTOR},
	{perkId: 'custom_themes', icon: 'palette', label: PERK_CUSTOM_THEMES_DESCRIPTOR},
];

const BYTES_PER_MEGABYTE = 1024 * 1024;

export function formatMegabytes(locale: string, bytes: number): string {
	return new Intl.NumberFormat(locale, {style: 'unit', unit: 'megabyte', unitDisplay: 'short'}).format(
		Math.floor(bytes / BYTES_PER_MEGABYTE),
	);
}

export function resolveUploadSizes(locale: string): {free: string; premium: string} {
	const perk = LIMIT_TIER_PERKS.find((candidate) => candidate.id === 'max_attachment_file_size');
	if (!perk || !isNumericTierPerk(perk) || !perk.limitKey) {
		return {
			free: formatMegabytes(locale, 25 * BYTES_PER_MEGABYTE),
			premium: formatMegabytes(locale, 500 * BYTES_PER_MEGABYTE),
		};
	}
	return {
		free: formatMegabytes(locale, Limits.getRestrictedValue(perk.limitKey, perk.restrictedValue)),
		premium: formatMegabytes(locale, Limits.getStockValue(perk.limitKey, perk.stockValue)),
	};
}

function resolveNumericValue(perk: LimitTierPerk, value: number, premium: boolean, locale: string): string {
	if (!isNumericTierPerk(perk)) return String(value);
	const resolved = perk.limitKey
		? premium
			? Limits.getStockValue(perk.limitKey, value)
			: Limits.getRestrictedValue(perk.limitKey, value)
		: value;
	if (perk.unit === 'bytes') {
		return formatMegabytes(locale, resolved);
	}
	return formatNumber(resolved, locale);
}

function buildRows(locale: string, translate: (descriptor: MessageDescriptor) => string): Array<PerkRow> {
	const rows: Array<PerkRow> = [];
	for (const definition of PERK_DEFINITIONS) {
		if (definition.perkId === 'custom_discriminator' && RuntimeConfig.usesUniqueUsernames) continue;
		const perk = LIMIT_TIER_PERKS.find((candidate) => candidate.id === definition.perkId);
		if (!perk) continue;
		if (isBooleanTierPerk(perk)) {
			rows.push({
				id: perk.id,
				icon: definition.icon,
				label: definition.label,
				free: {kind: 'boolean', value: perk.restrictedValue},
				premium: {kind: 'boolean', value: perk.stockValue},
			});
			continue;
		}
		if (isNumericTierPerk(perk)) {
			rows.push({
				id: perk.id,
				icon: definition.icon,
				label: definition.label,
				free: {kind: 'text', value: resolveNumericValue(perk, perk.restrictedValue, false, locale)},
				premium: {kind: 'text', value: resolveNumericValue(perk, perk.stockValue, true, locale)},
			});
			continue;
		}
		rows.push({
			id: perk.id,
			icon: definition.icon,
			label: definition.label,
			free: {kind: 'text', value: translate(PERK_VIDEO_QUALITY_FREE_DESCRIPTOR)},
			premium: {kind: 'text', value: translate(PERK_VIDEO_QUALITY_PREMIUM_DESCRIPTOR)},
		});
	}
	return rows;
}

function BooleanValue({available, highlighted}: {available: boolean; highlighted: boolean}) {
	const {i18n} = useLingui();
	return (
		<>
			<span className={styles.srOnly}>{i18n._(available ? AVAILABLE_DESCRIPTOR : NOT_AVAILABLE_DESCRIPTOR)}</span>
			<PlutoniumPageIcon
				name={available ? 'check' : 'cross'}
				className={clsx(
					styles.booleanIcon,
					!available
						? styles.booleanIconMissing
						: highlighted
							? styles.booleanIconHighlighted
							: styles.booleanIconMuted,
				)}
			/>
		</>
	);
}

function PerkValueView({value, highlighted}: {value: PerkValue; highlighted: boolean}) {
	if (value.kind === 'boolean') {
		return <BooleanValue available={value.value} highlighted={highlighted} />;
	}
	return <>{value.value}</>;
}

interface PlutoniumPageComparisonProps {
	footnoteId: string;
	onFootnoteClick: (event: React.MouseEvent<HTMLAnchorElement>) => void;
	actions: React.ReactNode;
}

export const PlutoniumPageComparison = observer(
	({footnoteId, onFootnoteClick, actions}: PlutoniumPageComparisonProps) => {
		const {i18n} = useLingui();
		const rows = buildRows(i18n.locale, (descriptor) => i18n._(descriptor));
		const premiumLabel = PREMIUM_PRODUCT_NAME;
		const renderLabel = (row: PerkRow) => (
			<>
				<span>{i18n._(row.label)}</span>
				{row.id === 'custom_discriminator' && (
					<FocusRing offset={-2}>
						<a
							href={`#${footnoteId}`}
							className={styles.footnoteMarker}
							onClick={onFootnoteClick}
							data-flx="premium.plutonium-page.comparison.footnote-marker"
						>
							<span aria-hidden="true">*</span>
							<span className={styles.srOnly}>{i18n._(TAG_FOOTNOTE_MARKER_DESCRIPTOR)}</span>
						</a>
					</FocusRing>
				)}
			</>
		);
		return (
			<section
				className={clsx(styles.glassPanel, styles.comparison)}
				aria-labelledby="plutonium-page-compare-heading"
				data-flx="premium.plutonium-page.comparison"
			>
				<h2
					id="plutonium-page-compare-heading"
					className={styles.titleHeading}
					data-flx="premium.plutonium-page.comparison.title"
				>
					{i18n._(COMPARE_TITLE_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
				</h2>
				<ul className={styles.perkCards} data-flx="premium.plutonium-page.comparison.cards">
					{rows.map((row) => (
						<li key={row.id} className={styles.perkCard} data-flx="premium.plutonium-page.comparison.card">
							<div className={styles.perkCardHead}>
								<PlutoniumPageIcon name={row.icon} className={styles.perkCardIcon} />
								<span className={styles.perkCardLabel}>{renderLabel(row)}</span>
							</div>
							<dl className={styles.perkCardValues}>
								<div className={styles.perkCell}>
									<dt className={styles.perkCellTerm}>{i18n._(FREE_COLUMN_DESCRIPTOR)}</dt>
									<dd className={styles.perkCellValue}>
										<PerkValueView value={row.free} highlighted={false} />
									</dd>
								</div>
								<div className={clsx(styles.perkCell, styles.perkCellPremium)}>
									<dt className={styles.perkCellTerm}>{premiumLabel}</dt>
									<dd className={styles.perkCellValue}>
										<PerkValueView value={row.premium} highlighted />
									</dd>
								</div>
							</dl>
						</li>
					))}
				</ul>
				<FocusRing offset={-2}>
					<section
						className={styles.tableWrap}
						aria-label={i18n._(COMPARE_TITLE_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						data-flx="premium.plutonium-page.comparison.table-wrap"
					>
						<table className={styles.table}>
							<thead>
								<tr>
									<th className={clsx(styles.tableHead, styles.tableHeadFeature)} scope="col">
										{i18n._(FEATURE_COLUMN_DESCRIPTOR)}
									</th>
									<th className={clsx(styles.tableHead, styles.tableHeadTier)} scope="col">
										{i18n._(FREE_COLUMN_DESCRIPTOR)}
									</th>
									<th className={clsx(styles.tableHead, styles.tableHeadTier, styles.tableHeadPremium)} scope="col">
										{premiumLabel}
									</th>
								</tr>
							</thead>
							<tbody>
								{rows.map((row) => (
									<tr key={row.id} className={styles.tableRow} data-flx="premium.plutonium-page.comparison.row">
										<th scope="row" className={styles.tableRowHead}>
											<span className={styles.tableRowLabel}>
												<PlutoniumPageIcon name={row.icon} className={styles.tableRowIcon} />
												<span>{renderLabel(row)}</span>
											</span>
										</th>
										<td className={styles.tableCell}>
											<PerkValueView value={row.free} highlighted={false} />
										</td>
										<td className={clsx(styles.tableCell, styles.tableCellPremium)}>
											<PerkValueView value={row.premium} highlighted />
										</td>
									</tr>
								))}
							</tbody>
						</table>
					</section>
				</FocusRing>
				{actions && (
					<div
						className={clsx(styles.actions, styles.comparisonActions)}
						data-flx="premium.plutonium-page.comparison.actions"
					>
						{actions}
					</div>
				)}
			</section>
		);
	},
);
