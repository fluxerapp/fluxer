// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
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
				free: {
					kind: 'boolean',
					value: perk.limitKey
						? Limits.hasRestrictedFeature(perk.limitKey, perk.restrictedValue)
						: perk.restrictedValue,
				},
				premium: {
					kind: 'boolean',
					value: perk.limitKey ? Limits.hasStockFeature(perk.limitKey, perk.stockValue) : perk.stockValue,
				},
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
		const freeHasHighQuality = perk.limitKey ? Limits.hasRestrictedFeature(perk.limitKey, false) : false;
		const premiumHasHighQuality = perk.limitKey ? Limits.hasStockFeature(perk.limitKey, true) : true;
		rows.push({
			id: perk.id,
			icon: definition.icon,
			label: definition.label,
			free: {
				kind: 'text',
				value: translate(
					freeHasHighQuality ? PERK_VIDEO_QUALITY_PREMIUM_DESCRIPTOR : PERK_VIDEO_QUALITY_FREE_DESCRIPTOR,
				),
			},
			premium: {
				kind: 'text',
				value: translate(
					premiumHasHighQuality ? PERK_VIDEO_QUALITY_PREMIUM_DESCRIPTOR : PERK_VIDEO_QUALITY_FREE_DESCRIPTOR,
				),
			},
		});
	}
	return rows;
}

function BooleanValue({available, highlighted}: {available: boolean; highlighted: boolean}) {
	const {i18n} = useLingui();
	return (
		<>
			<span className={styles.srOnly} data-flx="premium.plutonium-page.plutonium-page-comparison.boolean-value.sr-only">
				{i18n._(available ? AVAILABLE_DESCRIPTOR : NOT_AVAILABLE_DESCRIPTOR)}
			</span>
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
				data-flx="premium.plutonium-page.plutonium-page-comparison.boolean-value.boolean-icon"
			/>
		</>
	);
}

function PerkValueView({value, highlighted}: {value: PerkValue; highlighted: boolean}) {
	if (value.kind === 'boolean') {
		return (
			<BooleanValue
				available={value.value}
				highlighted={highlighted}
				data-flx="premium.plutonium-page.plutonium-page-comparison.perk-value-view.boolean-value"
			/>
		);
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
		const premiumLabel = getPremiumProductName();
		const renderLabel = (row: PerkRow) => (
			<>
				<span data-flx="premium.plutonium-page.plutonium-page-comparison.render-label.span">{i18n._(row.label)}</span>
				{row.id === 'custom_discriminator' && (
					<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page-comparison.render-label.focus-ring">
						<a
							href={`#${footnoteId}`}
							className={styles.footnoteMarker}
							onClick={onFootnoteClick}
							data-flx="premium.plutonium-page.comparison.footnote-marker"
						>
							<span aria-hidden="true" data-flx="premium.plutonium-page.plutonium-page-comparison.render-label.span--2">
								*
							</span>
							<span
								className={styles.srOnly}
								data-flx="premium.plutonium-page.plutonium-page-comparison.render-label.sr-only"
							>
								{i18n._(TAG_FOOTNOTE_MARKER_DESCRIPTOR)}
							</span>
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
					{i18n._(COMPARE_TITLE_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
				</h2>
				<ul className={styles.perkCards} data-flx="premium.plutonium-page.comparison.cards">
					{rows.map((row) => (
						<li key={row.id} className={styles.perkCard} data-flx="premium.plutonium-page.comparison.card">
							<div
								className={styles.perkCardHead}
								data-flx="premium.plutonium-page.plutonium-page-comparison.perk-card-head"
							>
								<PlutoniumPageIcon
									name={row.icon}
									className={styles.perkCardIcon}
									data-flx="premium.plutonium-page.plutonium-page-comparison.perk-card-icon"
								/>
								<span
									className={styles.perkCardLabel}
									data-flx="premium.plutonium-page.plutonium-page-comparison.perk-card-label"
								>
									{renderLabel(row)}
								</span>
							</div>
							<dl
								className={styles.perkCardValues}
								data-flx="premium.plutonium-page.plutonium-page-comparison.perk-card-values"
							>
								<div className={styles.perkCell} data-flx="premium.plutonium-page.plutonium-page-comparison.perk-cell">
									<dt
										className={styles.perkCellTerm}
										data-flx="premium.plutonium-page.plutonium-page-comparison.perk-cell-term"
									>
										{i18n._(FREE_COLUMN_DESCRIPTOR)}
									</dt>
									<dd
										className={styles.perkCellValue}
										data-flx="premium.plutonium-page.plutonium-page-comparison.perk-cell-value"
									>
										<PerkValueView
											value={row.free}
											highlighted={false}
											data-flx="premium.plutonium-page.plutonium-page-comparison.perk-value-view"
										/>
									</dd>
								</div>
								<div
									className={clsx(styles.perkCell, styles.perkCellPremium)}
									data-flx="premium.plutonium-page.plutonium-page-comparison.perk-cell--2"
								>
									<dt
										className={styles.perkCellTerm}
										data-flx="premium.plutonium-page.plutonium-page-comparison.perk-cell-term--2"
									>
										{premiumLabel}
									</dt>
									<dd
										className={styles.perkCellValue}
										data-flx="premium.plutonium-page.plutonium-page-comparison.perk-cell-value--2"
									>
										<PerkValueView
											value={row.premium}
											highlighted
											data-flx="premium.plutonium-page.plutonium-page-comparison.perk-value-view--2"
										/>
									</dd>
								</div>
							</dl>
						</li>
					))}
				</ul>
				<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page-comparison.focus-ring">
					<section
						className={styles.tableWrap}
						aria-label={i18n._(COMPARE_TITLE_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
						data-flx="premium.plutonium-page.comparison.table-wrap"
					>
						<table className={styles.table} data-flx="premium.plutonium-page.plutonium-page-comparison.table">
							<thead data-flx="premium.plutonium-page.plutonium-page-comparison.thead">
								<tr data-flx="premium.plutonium-page.plutonium-page-comparison.tr">
									<th
										className={clsx(styles.tableHead, styles.tableHeadFeature)}
										scope="col"
										data-flx="premium.plutonium-page.plutonium-page-comparison.table-head"
									>
										{i18n._(FEATURE_COLUMN_DESCRIPTOR)}
									</th>
									<th
										className={clsx(styles.tableHead, styles.tableHeadTier)}
										scope="col"
										data-flx="premium.plutonium-page.plutonium-page-comparison.table-head--2"
									>
										{i18n._(FREE_COLUMN_DESCRIPTOR)}
									</th>
									<th
										className={clsx(styles.tableHead, styles.tableHeadTier, styles.tableHeadPremium)}
										scope="col"
										data-flx="premium.plutonium-page.plutonium-page-comparison.table-head--3"
									>
										{premiumLabel}
									</th>
								</tr>
							</thead>
							<tbody data-flx="premium.plutonium-page.plutonium-page-comparison.tbody">
								{rows.map((row) => (
									<tr key={row.id} className={styles.tableRow} data-flx="premium.plutonium-page.comparison.row">
										<th
											scope="row"
											className={styles.tableRowHead}
											data-flx="premium.plutonium-page.plutonium-page-comparison.table-row-head"
										>
											<span
												className={styles.tableRowLabel}
												data-flx="premium.plutonium-page.plutonium-page-comparison.table-row-label"
											>
												<PlutoniumPageIcon
													name={row.icon}
													className={styles.tableRowIcon}
													data-flx="premium.plutonium-page.plutonium-page-comparison.table-row-icon"
												/>
												<span data-flx="premium.plutonium-page.plutonium-page-comparison.span">{renderLabel(row)}</span>
											</span>
										</th>
										<td
											className={styles.tableCell}
											data-flx="premium.plutonium-page.plutonium-page-comparison.table-cell"
										>
											<PerkValueView
												value={row.free}
												highlighted={false}
												data-flx="premium.plutonium-page.plutonium-page-comparison.perk-value-view--3"
											/>
										</td>
										<td
											className={clsx(styles.tableCell, styles.tableCellPremium)}
											data-flx="premium.plutonium-page.plutonium-page-comparison.table-cell--2"
										>
											<PerkValueView
												value={row.premium}
												highlighted
												data-flx="premium.plutonium-page.plutonium-page-comparison.perk-value-view--4"
											/>
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
