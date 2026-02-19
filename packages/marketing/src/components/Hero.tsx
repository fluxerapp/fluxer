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

import {Locales} from '@fluxer/constants/src/Locales';
import {FlagSvg} from '@fluxer/marketing/src/components/Flags';
import {HackernewsBanner} from '@fluxer/marketing/src/components/HackernewsBanner';
import {ArrowRightIcon} from '@fluxer/marketing/src/components/icons/ArrowRightIcon';
import {renderSecondaryButton, renderWithOverlay} from '@fluxer/marketing/src/components/PlatformDownloadButton';
import type {MarketingContext} from '@fluxer/marketing/src/MarketingContext';

interface HeroProps {
	ctx: MarketingContext;
}

export function Hero(props: HeroProps): JSX.Element {
	const {ctx} = props;

	return (
		<main class="flex flex-col items-center justify-center px-6 pt-36 pb-16 sm:px-8 md:px-12 md:pt-44 md:pb-20 lg:px-16 lg:pb-24 xl:px-20">
			<div class="max-w-4xl space-y-8 text-center">
				{ctx.locale === 'ja' ? (
					<div class="flex justify-center">
						<span class="font-bold text-3xl text-white">Fluxer（フラクサー）</span>
					</div>
				) : null}
				<div class="flex flex-wrap items-center justify-center gap-3 pb-2">
					<a
						href="https://blog.fluxer.app/how-i-built-fluxer-a-discord-like-chat-app/"
						target="_blank"
						rel="noopener noreferrer"
						class="inline-flex items-center gap-1.5 rounded-full bg-white/10 px-4 py-1.5 font-medium text-sm text-white transition-colors hover:bg-white/20"
					>
						{ctx.i18n.getMessage('launch.heading', ctx.locale)}
						<ArrowRightIcon class="h-3.5 w-3.5" />
					</a>
					<a
						href="https://blog.fluxer.app/roadmap-2026"
						target="_blank"
						rel="noopener noreferrer"
						class="inline-flex items-center gap-1.5 rounded-full bg-white/10 px-4 py-1.5 font-medium text-sm text-white transition-colors hover:bg-white/20"
					>
						{ctx.i18n.getMessage('launch.view_full_roadmap', ctx.locale)}
						<ArrowRightIcon class="h-3.5 w-3.5" />
					</a>
				</div>
				<h1 class="hero">{ctx.i18n.getMessage('general.tagline', ctx.locale)}</h1>
				<div class="-mt-4 flex items-center justify-center gap-2 font-medium text-sm text-white/80">
					<span class="inline-flex items-center gap-1.5">
						<FlagSvg locale={Locales.SV_SE} ctx={ctx} class="h-3.5 w-3.5 rounded-sm" />
						{ctx.i18n.getMessage('general.made_in_sweden', ctx.locale)}
					</span>
				</div>
				<p class="lead mx-auto max-w-2xl text-white/90">
					{ctx.i18n.getMessage('product_positioning.intro', ctx.locale)}
				</p>
				<div class="flex flex-col items-center justify-center gap-4 pt-4 sm:flex-row sm:items-stretch">
					{renderWithOverlay(ctx)}
					{renderSecondaryButton(
						ctx,
						`${ctx.appEndpoint}/channels/@me`,
						ctx.i18n.getMessage('download.open_in_browser', ctx.locale),
					)}
				</div>
				<HackernewsBanner ctx={ctx} />
			</div>
			<div class="mt-16 flex w-full max-w-6xl items-end justify-center gap-4 px-6 md:mt-24 md:gap-8">
				<div class="hidden w-full md:block md:w-4/5 lg:w-3/4">
					<picture>
						<source
							type="image/avif"
							srcset={`${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-480w.avif?v=4 480w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-768w.avif?v=4 768w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1024w.avif?v=4 1024w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1920w.avif?v=4 1920w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-2560w.avif?v=4 2560w`}
							sizes="(max-width: 768px) 100vw, 80vw"
						/>
						<source
							type="image/webp"
							srcset={`${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-480w.webp?v=4 480w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-768w.webp?v=4 768w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1024w.webp?v=4 1024w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1920w.webp?v=4 1920w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-2560w.webp?v=4 2560w`}
							sizes="(max-width: 768px) 100vw, 80vw"
						/>
						<img
							src={`${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1920w.png?v=4`}
							srcset={`${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-480w.png?v=4 480w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-768w.png?v=4 768w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1024w.png?v=4 1024w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-1920w.png?v=4 1920w, ${ctx.staticCdnEndpoint}/marketing/screenshots/desktop-2560w.png?v=4 2560w`}
							sizes="(max-width: 768px) 100vw, 80vw"
							alt={ctx.i18n.getMessage('platform_support.desktop.interface_label', ctx.locale)}
							class="aspect-video w-full rounded-lg border-2 border-white/50"
						/>
					</picture>
				</div>
				<div class="w-full max-w-[240px] md:w-1/6 md:max-w-none lg:w-1/6">
					<picture>
						<source
							type="image/avif"
							srcset={`${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-480w.avif?v=4 480w, ${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-768w.avif?v=4 768w`}
							sizes="(max-width: 768px) 240px, 17vw"
						/>
						<source
							type="image/webp"
							srcset={`${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-480w.webp?v=4 480w, ${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-768w.webp?v=4 768w`}
							sizes="(max-width: 768px) 240px, 17vw"
						/>
						<img
							src={`${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-768w.png?v=4`}
							srcset={`${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-480w.png?v=4 480w, ${ctx.staticCdnEndpoint}/marketing/screenshots/mobile-768w.png?v=4 768w`}
							sizes="(max-width: 768px) 240px, 17vw"
							alt={ctx.i18n.getMessage('platform_support.mobile.interface_label', ctx.locale)}
							class="aspect-[9/19] w-full rounded-xl border-2 border-white/50"
						/>
					</picture>
				</div>
			</div>
		</main>
	);
}
