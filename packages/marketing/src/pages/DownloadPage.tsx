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

import {defaultHeroPadding} from '@fluxer/marketing/src/components/HeroBase';
import {CoinsIcon} from '@fluxer/marketing/src/components/icons/CoinsIcon';
import {DownloadIcon} from '@fluxer/marketing/src/components/icons/DownloadIcon';
import {GithubIcon} from '@fluxer/marketing/src/components/icons/GithubIcon';
import {HeartIcon} from '@fluxer/marketing/src/components/icons/HeartIcon';
import {getSystemRequirements, renderDesktopButton} from '@fluxer/marketing/src/components/PlatformDownloadButton';
import {renderPwaInstallModal, renderPwaInstallTrigger} from '@fluxer/marketing/src/components/PwaInstallDialog';
import {Section} from '@fluxer/marketing/src/components/Section';
import type {MarketingContext} from '@fluxer/marketing/src/MarketingContext';
import {renderLayout} from '@fluxer/marketing/src/pages/Layout';
import {pageMeta} from '@fluxer/marketing/src/pages/layout/Meta';
import {href} from '@fluxer/marketing/src/UrlUtils';
import {GRADIENTS} from '@fluxer/ui/src/styles/Gradients';
import type {Context} from 'hono';

export async function renderDownloadPage(c: Context, ctx: MarketingContext): Promise<Response> {
	const content: ReadonlyArray<JSX.Element> = [
		renderHeroSection(ctx),
		renderMobileSection(ctx),
		renderSupportSection(ctx),
	];
	const meta = pageMeta(
		ctx.i18n.getMessage('download.download_fluxer', ctx.locale),
		ctx.i18n.getMessage('platform_support.desktop.download_desktop_intro', ctx.locale),
		'website',
	);
	const html = renderLayout(c, ctx, meta, content);
	return c.html(html);
}

function renderHeroSection(ctx: MarketingContext): JSX.Element {
	const windowsRequirements = getSystemRequirements(ctx, 'windows');
	const macosRequirements = getSystemRequirements(ctx, 'macos');
	const linuxRequirements = getSystemRequirements(ctx, 'linux');

	return (
		<section class={`flex flex-col items-center justify-center ${defaultHeroPadding()}`}>
			<div class="max-w-4xl space-y-8 text-center">
				<div class="flex justify-center">
					<div class="inline-flex h-28 w-28 items-center justify-center rounded-3xl bg-white/10 backdrop-blur-sm md:h-36 md:w-36">
						<DownloadIcon class="h-14 w-14 text-white md:h-18 md:w-18" />
					</div>
				</div>
				<h1 class="hero">{ctx.i18n.getMessage('download.download_fluxer', ctx.locale)}</h1>
				<p class="lead mx-auto max-w-2xl text-white/90">
					{ctx.i18n.getMessage('platform_support.desktop.available_on_desktop_and_web', ctx.locale)}
				</p>
			</div>

			<div class="mt-12 w-full max-w-3xl md:mt-16">
				<div class="flex flex-col flex-wrap items-stretch justify-center gap-6 sm:flex-row sm:items-start">
					<div class="flex w-full flex-col items-stretch sm:w-auto sm:items-start">
						{renderDesktopButton(ctx, 'windows', 'light', 'dl', true, true)}
						<p class="mt-2 w-full text-center text-white/50 text-xs">{windowsRequirements}</p>
					</div>
					<div class="flex w-full flex-col items-stretch sm:w-auto sm:items-start">
						{renderDesktopButton(ctx, 'macos', 'light', 'dl', true, true)}
						<p class="mt-2 w-full text-center text-white/50 text-xs">{macosRequirements}</p>
					</div>
					<div class="flex w-full flex-col items-stretch sm:w-auto sm:items-start">
						{renderDesktopButton(ctx, 'linux', 'light', 'dl', true, true)}
						<p class="invisible mt-2 w-full text-center text-white/50 text-xs">{linuxRequirements || '—'}</p>
					</div>
				</div>
			</div>
		</section>
	);
}

function renderMobileSection(ctx: MarketingContext): JSX.Element {
	return (
		<Section
			variant="light"
			title={ctx.i18n.getMessage('platform_support.mobile.mobile_apps_underway', ctx.locale)}
			description={ctx.i18n.getMessage('platform_support.mobile.mobile_browser_explainer', ctx.locale)}
		>
			<div class="mx-auto max-w-3xl">
				<div class="rounded-2xl border border-gray-200 bg-white p-8 shadow-md md:p-10">
					<ul class="space-y-4">
						<li class="flex items-start gap-3">
							<span class="mt-[.7em] h-1.5 w-1.5 shrink-0 rounded-full bg-[#4641D9]" />
							<span class="body-lg text-gray-900">
								{ctx.i18n.getMessage('platform_support.mobile.install_as_app.add_to_home', ctx.locale)}
							</span>
						</li>
						<li class="flex items-start gap-3">
							<span class="mt-[.7em] h-1.5 w-1.5 shrink-0 rounded-full bg-[#4641D9]" />
							<span class="body-lg text-gray-900">
								{ctx.i18n.getMessage('app.customization.app_icon_badges', ctx.locale)}
							</span>
						</li>
						<li class="flex items-start gap-3">
							<span class="mt-[.7em] h-1.5 w-1.5 shrink-0 rounded-full bg-[#4641D9]" />
							<span class="body-lg text-gray-900">
								{ctx.i18n.getMessage('platform_support.mobile.push_notifications', ctx.locale)}
							</span>
						</li>
					</ul>

					<div class="mt-8 flex justify-center">{renderPwaInstallTrigger(ctx)}</div>
					{renderPwaInstallModal(ctx)}

					<p class="mt-6 text-center text-gray-600 text-sm leading-relaxed">
						{ctx.i18n.getMessage('platform_support.mobile.not_full_replacement_yet', ctx.locale)}
					</p>
				</div>
			</div>
		</Section>
	);
}

function renderSupportSection(ctx: MarketingContext): JSX.Element {
	return (
		<section class={GRADIENTS.light}>
			<Section
				variant="cta"
				title={ctx.i18n.getMessage('donations.mobile_roadmap_sponsorship', ctx.locale)}
				description={ctx.i18n.getMessage('donations.why_support', ctx.locale)}
			>
				<div class="flex flex-col gap-6 sm:flex-row sm:justify-center">
					{renderSupportCard(
						href(ctx, '/plutonium'),
						<CoinsIcon class="h-8 w-8 text-white" />,
						ctx.i18n.getMessage('pricing_and_tiers.plutonium.tier_name', ctx.locale),
						ctx.i18n.getMessage('donations.support_future_development', ctx.locale),
						false,
					)}
					{renderSupportCard(
						href(ctx, '/donate'),
						<HeartIcon class="h-8 w-8 text-white" />,
						ctx.i18n.getMessage('donations.donate.label', ctx.locale),
						ctx.i18n.getMessage('donations.send_one_time_gift', ctx.locale),
						false,
					)}
					{renderSupportCard(
						'https://github.com/fluxerapp/fluxer',
						<GithubIcon class="h-8 w-8 text-white" />,
						ctx.i18n.getMessage('company_and_resources.source_and_contribution.contribute_on_github', ctx.locale),
						ctx.i18n.getMessage('company_and_resources.source_and_contribution.code_issues_docs_reviews', ctx.locale),
						true,
					)}
				</div>
			</Section>
		</section>
	);
}

function renderSupportCard(
	link: string,
	icon: JSX.Element,
	title: string,
	description: string,
	newTab: boolean,
): JSX.Element {
	const target = newTab ? '_blank' : undefined;
	const rel = newTab ? 'noopener noreferrer' : undefined;

	return (
		<a
			href={link}
			class="flex items-center gap-4 rounded-2xl border border-white/20 bg-white/10 p-6 backdrop-blur-sm transition hover:bg-white/20 md:p-8"
			target={target}
			rel={rel}
		>
			<div class="flex h-16 w-16 shrink-0 items-center justify-center rounded-2xl bg-white/20">{icon}</div>
			<div class="flex-1">
				<h3 class="title mb-1 text-white text-xl md:text-2xl">{title}</h3>
				<p class="text-sm text-white/80 md:text-base">{description}</p>
			</div>
		</a>
	);
}
