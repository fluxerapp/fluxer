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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {
	isBooleanPerk,
	isNumericPerk,
	isTextPerk,
	PLUTONIUM_PERKS,
	type PlutoniumPerk,
} from '@fluxer/constants/src/PlutoniumPerks';
import {FinalCtaSection} from '@fluxer/marketing/src/components/FinalCtaSection';
import {defaultHeroPadding, HeroBase} from '@fluxer/marketing/src/components/HeroBase';
import {CheckIcon} from '@fluxer/marketing/src/components/icons/CheckIcon';
import {CrossIcon} from '@fluxer/marketing/src/components/icons/CrossIcon';
import {FluxerPremiumIcon} from '@fluxer/marketing/src/components/icons/FluxerPremiumIcon';
import {Icon} from '@fluxer/marketing/src/components/icons/IconRegistry';
import type {MarketingContext} from '@fluxer/marketing/src/MarketingContext';
import {Currency, getCurrency, getFormattedPrice, PricingTier} from '@fluxer/marketing/src/PricingUtils';
import {renderLayout} from '@fluxer/marketing/src/pages/Layout';
import {pageMeta} from '@fluxer/marketing/src/pages/layout/Meta';
import {formatNumber} from '@fluxer/marketing/src/utils/NumberFormatUtils';
import type {Context} from 'hono';

export async function renderPlutoniumPage(c: Context, ctx: MarketingContext): Promise<Response> {
	const content: ReadonlyArray<JSX.Element> = [
		renderHeroSection(ctx),
		renderComparisonSection(ctx),
		renderFeaturesSection(ctx),
		renderSelfHostingSection(ctx),
		renderCtaSection(ctx),
	];
	const meta = pageMeta(
		ctx.i18n.getMessage('pricing_and_tiers.plutonium.tier_name', ctx.locale),
		ctx.i18n.getMessage('pricing_and_tiers.plutonium.upgrade_pitch', ctx.locale),
		'website',
	);
	const html = renderLayout(c, ctx, meta, content);
	return c.html(html);
}

function renderHeroSection(ctx: MarketingContext): JSX.Element {
	const monthlyPrice = getFormattedPrice(PricingTier.Monthly, ctx.countryCode);
	const yearlyPrice = getFormattedPrice(PricingTier.Yearly, ctx.countryCode);

	return (
		<HeroBase
			icon={<FluxerPremiumIcon class="h-14 w-14 text-white md:h-18 md:w-18" fillColor="#4641D9" />}
			title={ctx.i18n.getMessage('pricing_and_tiers.plutonium.tier_name', ctx.locale)}
			description={ctx.i18n.getMessage('pricing_and_tiers.plutonium.higher_limits_and_early_access', ctx.locale)}
			extraContent={
				<div>
					<p class="body-lg mx-auto mt-6 mb-10 max-w-3xl text-white/70 md:mb-12">
						{ctx.i18n.getMessage('pricing_and_tiers.plutonium.benefits_note_official_instance_only', ctx.locale)}
					</p>
					<div class="mb-8 flex flex-col items-center justify-center gap-3 sm:flex-row sm:gap-4">
						<span class="font-bold text-3xl md:text-4xl">{`${monthlyPrice}${ctx.i18n.getMessage('pricing_and_tiers.billing.per_month', ctx.locale)}`}</span>
						<span class="text-lg text-white/80">{ctx.i18n.getMessage('general.or', ctx.locale)}</span>
						<span class="font-bold text-3xl md:text-4xl">{`${yearlyPrice}${ctx.i18n.getMessage('pricing_and_tiers.billing.per_year_short', ctx.locale)}`}</span>
						<span class="inline-flex items-center rounded-xl bg-white/20 px-4 py-2 font-semibold text-base backdrop-blur-sm md:text-lg">
							{ctx.i18n.getMessage('pricing_and_tiers.billing.save_percent', ctx.locale)}
						</span>
					</div>
				</div>
			}
			customPadding={defaultHeroPadding()}
		/>
	);
}

function getPerkLabel(ctx: MarketingContext, perk: PlutoniumPerk): string {
	const labelMap: Record<string, string> = {
		custom_4_digit_username_tag: ctx.i18n.getMessage(
			'pricing_and_tiers.plutonium.features.custom_4_digit_username_tag',
			ctx.locale,
		),
		per_community_profiles: ctx.i18n.getMessage(
			'pricing_and_tiers.plutonium.features.per_community_profiles',
			ctx.locale,
		),
		message_scheduling: ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.message_scheduling', ctx.locale),
		profile_badge: ctx.i18n.getMessage('misc_labels.profile_badge', ctx.locale),
		custom_video_backgrounds: ctx.i18n.getMessage(
			'pricing_and_tiers.plutonium.features.custom_video_backgrounds',
			ctx.locale,
		),
		entrance_sounds: ctx.i18n.getMessage('app.customization.custom_sounds.entrance_sounds', ctx.locale),
		communities: ctx.i18n.getMessage('app.communities.title', ctx.locale),
		message_character_limit: ctx.i18n.getMessage(
			'pricing_and_tiers.plutonium.features.message_character_limit',
			ctx.locale,
		),
		bookmarked_messages: ctx.i18n.getMessage('app.messaging.features.bookmarked_messages', ctx.locale),
		file_upload_size: ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.file_upload_size', ctx.locale),
		emoji_sticker_packs: ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.emoji_sticker_packs', ctx.locale),
		saved_media: ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.saved_media', ctx.locale),
		use_animated_emojis: ctx.i18n.getMessage('app.customization.use_animated_emojis', ctx.locale),
		global_emoji_sticker_access: ctx.i18n.getMessage(
			'pricing_and_tiers.plutonium.features.global_emoji_sticker_access',
			ctx.locale,
		),
		video_quality: ctx.i18n.getMessage('app.voice_and_video.features.video_quality', ctx.locale),
		animated_avatars_and_banners: ctx.i18n.getMessage(
			'app.customization.animated_profile.animated_avatars_and_banners',
			ctx.locale,
		),
		early_access: ctx.i18n.getMessage('beta_and_access.early_access.label', ctx.locale),
		custom_themes: ctx.i18n.getMessage('app.customization.custom_themes', ctx.locale),
	};
	return labelMap[perk.i18nKey] || perk.i18nKey;
}

function getStatusBadge(ctx: MarketingContext, perk: PlutoniumPerk): string | null {
	if (perk.status === 'beta') {
		return ctx.i18n.getMessage('beta_and_access.beta_label', ctx.locale);
	}
	if (perk.status === 'coming_soon') {
		return ctx.i18n.getMessage('general.coming_soon.label', ctx.locale);
	}
	return null;
}

function formatBytes(bytes: number): string {
	const mb = bytes / (1024 * 1024);
	return `${mb} MB`;
}

function formatPerkNumericValue(value: number, unit?: string): string {
	if (unit === 'bytes') {
		return formatBytes(value);
	}
	return formatNumber(value);
}

function renderComparisonSection(ctx: MarketingContext): JSX.Element {
	const currency = getCurrency(ctx.countryCode);
	const freePrice = currency === Currency.EUR ? '€0' : '$0';
	const monthlyPrice = getFormattedPrice(PricingTier.Monthly, ctx.countryCode);
	const yearlyPrice = getFormattedPrice(PricingTier.Yearly, ctx.countryCode);

	const renderPerkRow = (perk: PlutoniumPerk): JSX.Element | null => {
		const label = getPerkLabel(ctx, perk);
		const badge = getStatusBadge(ctx, perk);

		if (isBooleanPerk(perk)) {
			if (perk.status === 'coming_soon') {
				return comparisonRow(
					ctx,
					label,
					ctx.i18n.getMessage('general.not_available', ctx.locale),
					ctx.i18n.getMessage('general.coming_soon.label', ctx.locale),
					badge,
				);
			}
			return comparisonCheckRow(ctx, label, perk.freeValue, perk.plutoniumValue, badge);
		}

		if (isNumericPerk(perk)) {
			return comparisonRow(
				ctx,
				label,
				formatPerkNumericValue(perk.freeValue, perk.unit),
				formatPerkNumericValue(perk.plutoniumValue, perk.unit),
				badge,
			);
		}

		if (isTextPerk(perk)) {
			const freeValue =
				perk.freeValueI18nKey === 'video_quality_free'
					? ctx.i18n.getMessage('app.voice_and_video.features.video_quality_free', ctx.locale)
					: perk.freeValueI18nKey;
			const premiumValue =
				perk.plutoniumValueI18nKey === 'video_quality_premium'
					? ctx.i18n.getMessage('app.voice_and_video.features.video_quality_premium', ctx.locale)
					: perk.plutoniumValueI18nKey;
			return comparisonRow(ctx, label, freeValue, premiumValue, badge);
		}

		return null;
	};

	return (
		<section class="bg-gradient-to-b from-white to-gray-50 px-6 py-24 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20">
			<div class="mx-auto max-w-5xl">
				<h2 class="display mb-16 text-center text-4xl text-black md:mb-20 md:text-5xl lg:text-6xl">
					{ctx.i18n.getMessage('pricing_and_tiers.free.comparison_label', ctx.locale)}
				</h2>
				<div class="mx-auto mb-16 grid max-w-4xl grid-cols-1 gap-8 md:mb-20 md:grid-cols-2 md:gap-10">
					<div class="rounded-3xl border-2 border-gray-200 bg-white p-10 text-center shadow-lg md:p-12">
						<h3 class="title mb-4 text-2xl text-black md:text-3xl">
							{ctx.i18n.getMessage('pricing_and_tiers.free.label', ctx.locale)}
						</h3>
						<p class="mb-3 font-bold text-4xl text-gray-900 md:text-5xl">{freePrice}</p>
						<p class="body-lg text-gray-600">{ctx.i18n.getMessage('pricing_and_tiers.billing.forever', ctx.locale)}</p>
					</div>
					<div class="relative rounded-3xl border-2 border-[#4641D9] bg-gradient-to-br from-[#4641D9]/5 to-[#6b5ce7]/5 p-10 text-center shadow-xl md:p-12">
						<div class="label absolute -top-4 left-1/2 -translate-x-1/2 rounded-xl bg-[#4641D9] px-4 py-2 text-white shadow-md">
							{ctx.i18n.getMessage('pricing_and_tiers.billing.most_popular', ctx.locale)}
						</div>
						<h3 class="title mb-4 text-2xl text-black md:text-3xl">
							{ctx.i18n.getMessage('pricing_and_tiers.plutonium.tier_name', ctx.locale)}
						</h3>
						<p class="mb-3 font-bold text-4xl text-[#4641D9] md:text-5xl">{`${monthlyPrice}${ctx.i18n.getMessage('pricing_and_tiers.billing.per_month', ctx.locale)}`}</p>
						<p class="body-lg text-gray-700">
							{ctx.i18n.getMessage('general.or', ctx.locale)} {yearlyPrice}
							{ctx.i18n.getMessage('pricing_and_tiers.billing.per_year_full', ctx.locale)}
						</p>
					</div>
				</div>
				<div class="overflow-x-auto">
					<table class="w-full border-collapse rounded-lg border border-gray-200" style="table-layout: fixed">
						<thead class="bg-gray-50">
							<tr>
								<th class="label w-1/2 border-gray-200 border-b px-4 py-3 text-left text-black">
									{ctx.i18n.getMessage('misc_labels.feature', ctx.locale)}
								</th>
								<th class="label w-1/4 border-gray-200 border-b px-2 py-3 text-center text-black text-xs sm:px-3 sm:text-sm">
									{ctx.i18n.getMessage('pricing_and_tiers.free.label', ctx.locale)}
								</th>
								<th class="label w-1/4 border-gray-200 border-b px-2 py-3 text-center text-[#4641D9] text-xs sm:px-3 sm:text-sm">
									{ctx.i18n.getMessage('pricing_and_tiers.plutonium.tier_name', ctx.locale)}
								</th>
							</tr>
						</thead>
						<tbody>{PLUTONIUM_PERKS.map(renderPerkRow)}</tbody>
					</table>
				</div>
				<div class="mt-12 text-center md:mt-16">
					<a
						href={`${ctx.appEndpoint}/channels/@me`}
						class="label inline-block rounded-xl bg-[#4641D9] px-10 py-5 text-lg text-white shadow-lg transition hover:bg-[#3d38c7] md:px-12 md:py-6 md:text-xl"
					>
						{ctx.i18n.getMessage('pricing_and_tiers.plutonium.get_plutonium', ctx.locale)}
					</a>
				</div>
			</div>
		</section>
	);
}

function comparisonRow(
	_ctx: MarketingContext,
	feature: string,
	freeValue: string,
	plutoniumValue: string,
	badge: string | null,
): JSX.Element {
	const badgeNode = badge ? (
		<span class="caption inline-flex items-center rounded-full border border-[#4641D9] px-3 py-1 font-semibold text-[#4641D9] text-xs uppercase tracking-wider">
			{badge}
		</span>
	) : null;

	return (
		<tr class="border-gray-100 border-b">
			<td class="body px-4 py-3 text-gray-900">
				<div class="flex flex-wrap items-center gap-2">
					<span>{feature}</span>
					{badgeNode}
				</div>
			</td>
			<td class="body px-2 py-3 text-center text-gray-600 text-xs sm:px-3 sm:text-sm">{freeValue}</td>
			<td class="label px-2 py-3 text-center text-[#4641D9] text-xs sm:px-3 sm:text-sm">{plutoniumValue}</td>
		</tr>
	);
}

function comparisonCheckRow(
	_ctx: MarketingContext,
	feature: string,
	freeHas: boolean,
	plutoniumHas: boolean,
	badge: string | null,
): JSX.Element {
	const badgeNode = badge ? (
		<span class="caption inline-flex items-center rounded-full border border-[#4641D9] px-3 py-1 font-semibold text-[#4641D9] text-xs uppercase tracking-wider">
			{badge}
		</span>
	) : null;

	return (
		<tr class="border-gray-100 border-b">
			<td class="body px-4 py-3 text-gray-900">
				<div class="flex flex-wrap items-center gap-2">
					<span>{feature}</span>
					{badgeNode}
				</div>
			</td>
			<td class="px-3 py-3 text-center">
				{freeHas ? (
					<CheckIcon class="mx-auto h-5 w-5 text-green-600" />
				) : (
					<CrossIcon class="mx-auto h-5 w-5 text-gray-400" />
				)}
			</td>
			<td class="px-3 py-3 text-center">
				{plutoniumHas ? (
					<CheckIcon class="mx-auto h-5 w-5 text-[#4641D9]" />
				) : (
					<CrossIcon class="mx-auto h-5 w-5 text-gray-400" />
				)}
			</td>
		</tr>
	);
}

function renderFeaturesSection(ctx: MarketingContext): JSX.Element {
	return (
		<section class="bg-white px-6 py-24 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20">
			<div class="mx-auto max-w-7xl">
				<h2 class="display mb-16 text-center text-4xl text-black md:mb-20 md:text-5xl lg:text-6xl">
					{ctx.i18n.getMessage('pricing_and_tiers.plutonium.get_more_with_plutonium', ctx.locale)}
				</h2>
				<div class="grid grid-cols-1 gap-8 md:grid-cols-2 md:gap-10 lg:grid-cols-3">
					{renderFeatureCard(
						ctx,
						'hash',
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.custom_username_tag', ctx.locale),
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.choose_custom_4_digit_tag', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'user_circle',
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.per_community_profiles', ctx.locale),
						ctx.i18n.getMessage('app.profiles_identity.customise_per_community', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'calendar_check',
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.message_scheduling', ctx.locale),
						ctx.i18n.getMessage('app.messaging.features.message_scheduling.description', ctx.locale),
						ctx.i18n.getMessage('general.coming_soon.label', ctx.locale),
					)}
					{renderFeatureCard(
						ctx,
						'gif',
						ctx.i18n.getMessage('app.customization.animated_profile.animated_avatars_and_banners', ctx.locale),
						ctx.i18n.getMessage('app.customization.animated_profile.stand_out_animated_profile', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'smiley',
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.global_emoji_sticker_access', ctx.locale),
						ctx.i18n.getMessage('app.customization.global_emoji_and_sticker_access', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'video_camera',
						ctx.i18n.getMessage('app.voice_and_video.features.up_to_4k_video_quality', ctx.locale),
						ctx.i18n.getMessage('app.voice_and_video.features.stream_4k_60fps', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'video',
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.features.custom_video_backgrounds', ctx.locale),
						ctx.i18n.getMessage('app.voice_and_video.features.video_backgrounds.description', ctx.locale),
						ctx.i18n.getMessage('beta_and_access.beta_label', ctx.locale),
					)}
					{renderFeatureCard(
						ctx,
						'user_plus',
						ctx.i18n.getMessage('app.customization.custom_sounds.entrance_sounds', ctx.locale),
						ctx.i18n.getMessage('app.customization.custom_sounds.set_personalized_join_sounds', ctx.locale),
						ctx.i18n.getMessage('beta_and_access.beta_label', ctx.locale),
					)}
					{renderFeatureCard(
						ctx,
						'arrow_up',
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.higher_limits_everywhere', ctx.locale),
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.feature_highlights', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'fluxer_premium',
						ctx.i18n.getMessage('misc_labels.profile_badge', ctx.locale),
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.show_off_status_badge', ctx.locale),
						null,
					)}
					{renderFeatureCard(
						ctx,
						'rocket',
						ctx.i18n.getMessage('beta_and_access.early_access.label', ctx.locale),
						ctx.i18n.getMessage('beta_and_access.early_access.be_first_to_try', ctx.locale),
						null,
					)}
				</div>
			</div>
		</section>
	);
}

type FeatureIconName =
	| 'sparkle'
	| 'video'
	| 'video_camera'
	| 'user_circle'
	| 'user_plus'
	| 'fluxer_premium'
	| 'calendar_check'
	| 'rocket'
	| 'hash'
	| 'gif'
	| 'arrow_up'
	| 'smiley';

function renderFeatureCard(
	_ctx: MarketingContext,
	iconName: FeatureIconName,
	title: string,
	description: string,
	badge: string | null,
): JSX.Element {
	const icon = getFeatureIcon(iconName);
	const badgeNode = badge ? (
		<div class="caption absolute top-4 right-4 rounded-full bg-[#4641D9] px-3 py-1 font-semibold text-white text-xs uppercase tracking-wide shadow-lg">
			{badge}
		</div>
	) : null;

	return (
		<div class="relative rounded-3xl border border-gray-100 bg-gray-50 p-8 shadow-md md:p-10">
			{badgeNode}
			<div class="mb-5 inline-flex h-16 w-16 items-center justify-center rounded-2xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 md:h-20 md:w-20">
				{icon}
			</div>
			<h3 class="title mb-3 text-black text-xl md:text-2xl">{title}</h3>
			<p class="body-lg text-gray-700 leading-relaxed">{description}</p>
		</div>
	);
}

function getFeatureIcon(iconName: FeatureIconName): JSX.Element {
	return <Icon name={iconName} class="h-8 w-8 text-[#4641D9] md:h-10 md:w-10" />;
}

function renderSelfHostingSection(ctx: MarketingContext): JSX.Element {
	const currency = getCurrency(ctx.countryCode);
	const freePrice = currency === Currency.EUR ? '€0' : '$0';
	const operatorPrice = currency === Currency.EUR ? '€199' : '$199';

	return (
		<section
			id="self-hosting"
			class="bg-gradient-to-b from-gray-50 to-white px-6 py-24 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20"
			style="scroll-margin-top: 8rem"
		>
			<div class="mx-auto max-w-7xl">
				<div class="mb-16 text-center md:mb-20">
					<h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl">
						{ctx.i18n.getMessage('product_positioning.self_hosting.label', ctx.locale)}
					</h2>
					<p class="lead mx-auto mb-3 max-w-3xl text-gray-700 text-xl md:text-2xl">
						{ctx.i18n.getMessage('product_positioning.free_and_open_source', ctx.locale)}
					</p>
					<p class="body-lg mx-auto max-w-3xl text-gray-600">
						{ctx.i18n.getMessage('product_positioning.self_hosting.operator_pass.note_optional', ctx.locale)}
					</p>
				</div>
				<div class="mx-auto mb-16 grid max-w-5xl grid-cols-1 gap-10 md:mb-20 md:grid-cols-2 md:gap-12">
					<div class="rounded-3xl border-2 border-gray-200 bg-white p-10 shadow-lg md:p-12">
						<div class="mb-4 flex justify-center">
							<Icon name="globe" class="h-16 w-16 text-gray-400" />
						</div>
						<h3 class="title mb-2 text-center text-black text-xl md:text-2xl">
							{ctx.i18n.getMessage('product_positioning.self_hosting.free_self_hosting', ctx.locale)}{' '}
						</h3>
						<div class="mb-6 text-center">
							<span class="display text-4xl text-black md:text-5xl">{freePrice}</span>
							<span class="body-lg text-gray-600">
								{ctx.i18n.getMessage('pricing_and_tiers.billing.per_forever', ctx.locale)}
							</span>
						</div>
						<div class="mb-6 space-y-3">
							{renderBenefitItem(ctx, ctx.i18n.getMessage('misc_labels.unlimited_users', ctx.locale))}
							{renderBenefitItem(
								ctx,
								ctx.i18n.getMessage('pricing_and_tiers.free.full_access_to_all_features', ctx.locale),
							)}
							{renderBenefitItem(
								ctx,
								ctx.i18n.getMessage('product_positioning.self_hosting.connect_from_any_client', ctx.locale),
							)}
							{renderBenefitItem(ctx, ctx.i18n.getMessage('product_positioning.open_source.license', ctx.locale))}
							{renderBenefitItem(ctx, ctx.i18n.getMessage('app.communities.community_support', ctx.locale))}
						</div>
					</div>
					<div class="relative rounded-3xl border-2 border-[#4641D9] bg-white p-10 shadow-xl md:p-12">
						<div class="label absolute -top-4 left-1/2 -translate-x-1/2 rounded-xl bg-[#4641D9] px-4 py-2 text-white shadow-md">
							{ctx.i18n.getMessage('general.coming_soon.label', ctx.locale)}
						</div>
						<div class="mb-4 flex justify-center">
							<Icon name="globe" class="h-16 w-16 text-[#4641D9]" />
						</div>
						<h3 class="title mb-2 text-center text-black text-xl md:text-2xl">
							{ctx.i18n.getMessage('product_positioning.self_hosting.operator_pass.label', ctx.locale)}{' '}
						</h3>
						<div class="mb-6 text-center">
							<span class="display text-4xl text-black md:text-5xl">{operatorPrice}</span>
							<span class="body-lg block text-gray-600">
								{ctx.i18n.getMessage('pricing_and_tiers.visionary.one_time_purchase.label', ctx.locale)}
							</span>
						</div>
						<div class="mb-6 space-y-3">
							{renderBenefitItem(
								ctx,
								ctx.i18n.getMessage('pricing_and_tiers.free.everything_in_free_plus', ctx.locale),
							)}
							{renderBenefitItem(
								ctx,
								ctx.i18n.getMessage(
									'product_positioning.self_hosting.operator_pass.access_to_operators_community',
									ctx.locale,
								),
							)}
							{renderBenefitItem(ctx, ctx.i18n.getMessage('misc_labels.direct_team_support', ctx.locale))}
							{renderBenefitItem(ctx, ctx.i18n.getMessage('donations.support_future_development', ctx.locale))}
						</div>
					</div>
				</div>
			</div>
		</section>
	);
}

function renderBenefitItem(_ctx: MarketingContext, text: string): JSX.Element {
	return (
		<div class="flex items-start gap-3">
			<CheckIcon class="mt-0.5 h-5 w-5 shrink-0 text-[#4641D9]" />
			<span class="body text-gray-700">{text}</span>
		</div>
	);
}

function renderCtaSection(ctx: MarketingContext): JSX.Element {
	return <FinalCtaSection ctx={ctx} />;
}
