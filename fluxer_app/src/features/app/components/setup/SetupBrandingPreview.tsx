// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SetupBrandingPreview.module.css';
import type {SetupBrandingAssetKind} from '@app/features/app/components/setup/SetupWizardClient';
import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import {contrastRatio, getInstanceBrandVariables} from '@app/features/theme/utils/InstanceBrandColors';
import {FluxerWordmark} from '@app/features/ui/components/icons/FluxerWordmark';
import {SegmentedTabs} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GlobeIcon, LockSimpleIcon, MinusIcon, SquareIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useEffect, useMemo, useRef, useState} from 'react';

export type BrandingPreviewTab = 'sign_in' | 'phone' | 'desktop' | 'instances';
export type BrandingPreviewTarget = SetupBrandingAssetKind | 'theme_color';
type DesktopPlatform = 'controls' | 'macos';

const PREVIEW_TABS_DESCRIPTOR = msg({
	message: 'Preview',
	comment: 'Accessible label for the tabs that switch between branding previews in the setup wizard.',
});
const TAB_SIGN_IN_DESCRIPTOR = msg({
	message: 'Sign-in page',
	comment: 'Setup wizard branding preview tab that shows the sign-in page on a computer.',
});
const TAB_PHONE_DESCRIPTOR = msg({
	message: 'Phone',
	comment: 'Setup wizard branding preview tab that shows the sign-in page on a phone.',
});
const TAB_DESKTOP_DESCRIPTOR = msg({
	message: 'Desktop app',
	comment: 'Setup wizard branding preview tab that shows the desktop app title bar.',
});
const TAB_INSTANCES_DESCRIPTOR = msg({
	message: 'Instance list',
	comment:
		'Setup wizard branding preview tab that shows how the instance appears in the account switcher and instance list.',
});
const CAPTION_SIGN_IN_DESCRIPTOR = msg({
	message: 'The sign-in page in a browser on a computer. The desktop app shows the same page.',
	comment: 'Caption under the sign-in page preview in the setup wizard branding step.',
});
const CAPTION_PHONE_DESCRIPTOR = msg({
	message: 'The sign-in page on a phone, in a mobile browser or the Fluxer mobile app.',
	comment: 'Caption under the phone preview in the setup wizard branding step.',
});
const CAPTION_DESKTOP_DESCRIPTOR = msg({
	message: 'The wordmark in the desktop app title bar, which follows the light or dark theme each person picks.',
	comment: 'Caption under the desktop app title bar preview in the setup wizard branding step.',
});
const CAPTION_INSTANCES_DESCRIPTOR = msg({
	message:
		'People who use more than one instance see this in the account switcher and when they pick an instance to sign in to.',
	comment: 'Caption under the instance list preview in the setup wizard branding step.',
});
const WINDOWS_LINUX_DESCRIPTOR = msg({
	message: 'Windows and Linux',
	comment: 'Label above the Windows and Linux title bar preview in the setup wizard branding step.',
});
const MACOS_DESCRIPTOR = msg({
	message: 'macOS',
	comment: 'Label above the macOS title bar preview in the setup wizard branding step.',
});
const DARK_THEME_DESCRIPTOR = msg({
	message: 'Dark theme',
	comment: 'Label for the dark theme title bar preview in the setup wizard branding step.',
});
const LIGHT_THEME_DESCRIPTOR = msg({
	message: 'Light theme',
	comment: 'Label for the light theme title bar preview in the setup wizard branding step.',
});

export interface BrandingPreviewDraft {
	productName: string;
	themeColor: string | null;
	sources: Readonly<Record<SetupBrandingAssetKind, string | null>>;
}

interface BrandingPreviewProps {
	draft: BrandingPreviewDraft;
	tab: BrandingPreviewTab;
	highlight: BrandingPreviewTarget | null;
	caption: React.ReactNode;
	onTabChange: (tab: BrandingPreviewTab) => void;
	onTargetSelect: (target: BrandingPreviewTarget) => void;
}

interface ResolvedPreviewBrand {
	productName: string;
	usesDefaultName: boolean;
	logoUrl: string | null;
	showBundledLogo: boolean;
	wordmarkUrl: string | null;
	backdropUrl: string | null;
	faviconUrl: string | null;
	faviconTarget: 'favicon' | 'favicon_dark';
	symbolUrl: string | null;
	accent: string | null;
	textOnAccent: string | null;
	toolbarColor: string | null;
	toolbarText: string;
	host: string;
}

const DEFAULT_ACCENT = '#4641d9';
const TOOLBAR_LIGHT_TEXT = [0xf2, 0xf2, 0xf5] as const;
const TOOLBAR_DARK_TEXT = [0x18, 0x17, 0x1f] as const;

function toolbarTextFor(color: string | null): string {
	if (color == null) return 'rgb(242 242 245)';
	const value = Number.parseInt(color.slice(1, 7), 16);
	const rgb = [(value >>> 16) & 0xff, (value >>> 8) & 0xff, value & 0xff] as const;
	return contrastRatio(rgb, TOOLBAR_LIGHT_TEXT) >= contrastRatio(rgb, TOOLBAR_DARK_TEXT)
		? 'rgb(242 242 245)'
		: 'rgb(24 23 31)';
}

function resolvePreviewBrand(draft: BrandingPreviewDraft): ResolvedPreviewBrand {
	const {sources} = draft;
	const productName = draft.productName.trim() || PRODUCT_NAME;
	const usesDefaultName = productName === PRODUCT_NAME;
	const logoUrl = sources.logo ?? sources.icon ?? sources.symbol;
	const accentVariables = getInstanceBrandVariables(draft.themeColor ?? DEFAULT_ACCENT, 'dark', 1);
	return {
		productName,
		usesDefaultName,
		logoUrl,
		showBundledLogo: logoUrl == null && usesDefaultName,
		wordmarkUrl: sources.wordmark,
		backdropUrl: sources.sign_in_backdrop,
		faviconUrl: sources.favicon_dark ?? sources.favicon ?? sources.icon,
		faviconTarget: sources.favicon_dark != null ? 'favicon_dark' : 'favicon',
		symbolUrl: sources.symbol ?? sources.icon ?? sources.logo,
		accent: accentVariables?.['--instance-brand-primary'] ?? null,
		textOnAccent: accentVariables?.['--instance-text-on-brand-primary'] ?? null,
		toolbarColor: draft.themeColor,
		toolbarText: toolbarTextFor(draft.themeColor),
		host: window.location.host,
	};
}

function phoneShowsLogo(brand: ResolvedPreviewBrand): boolean {
	return brand.wordmarkUrl == null && !brand.usesDefaultName && brand.logoUrl != null;
}

export function brandingPreviewTabsFor(
	target: BrandingPreviewTarget,
	draft: BrandingPreviewDraft,
): ReadonlyArray<BrandingPreviewTab> {
	switch (target) {
		case 'theme_color':
			return ['sign_in', 'phone'];
		case 'logo':
			return phoneShowsLogo(resolvePreviewBrand(draft)) ? ['sign_in', 'phone'] : ['sign_in'];
		case 'wordmark':
			return ['sign_in', 'phone', 'desktop'];
		case 'sign_in_backdrop':
			return ['sign_in', 'phone'];
		case 'icon':
			return ['sign_in', 'instances'];
		case 'symbol':
			return ['instances'];
		case 'favicon':
		case 'favicon_dark':
			return ['sign_in'];
	}
}

interface HotspotProps {
	target: BrandingPreviewTarget;
	highlightTargets?: ReadonlyArray<BrandingPreviewTarget>;
	highlight: BrandingPreviewTarget | null;
	onSelect: (target: BrandingPreviewTarget) => void;
	className?: string;
	style?: React.CSSProperties;
	children?: React.ReactNode;
	'data-flx': string;
}

function Hotspot({
	target,
	highlightTargets = [target],
	highlight,
	onSelect,
	className,
	style,
	children,
	'data-flx': dataFlx,
}: HotspotProps) {
	return (
		<button
			type="button"
			tabIndex={-1}
			className={clsx(
				styles.hotspot,
				className,
				highlight != null && highlightTargets.includes(highlight) && styles.highlighted,
			)}
			style={style}
			onClick={() => onSelect(target)}
			data-flx={dataFlx}
		>
			{children}
		</button>
	);
}

interface SceneProps {
	brand: ResolvedPreviewBrand;
	highlight: BrandingPreviewTarget | null;
	onSelect: (target: BrandingPreviewTarget) => void;
}

function PreviewWordmark({brand, className}: {brand: ResolvedPreviewBrand; className: string}) {
	const branding = useMemo(
		() => ({productName: brand.productName, wordmarkUrl: brand.wordmarkUrl}),
		[brand.productName, brand.wordmarkUrl],
	);
	return (
		<FluxerWordmark
			key={brand.wordmarkUrl ?? brand.productName}
			className={className}
			branding={branding}
			aria-hidden="true"
			data-flx="app.setup.setup-branding-preview.preview-wordmark.fluxer-wordmark"
		/>
	);
}

function PreviewLogo({brand, className}: {brand: ResolvedPreviewBrand; className: string}) {
	if (brand.logoUrl != null) {
		return (
			<img
				className={className}
				src={brand.logoUrl}
				alt=""
				draggable={false}
				data-flx="app.setup.setup-branding-preview.preview-logo.img"
			/>
		);
	}
	if (brand.showBundledLogo) {
		return (
			<FluxerLogoAsset
				className={className}
				aria-hidden="true"
				data-flx="app.setup.setup-branding-preview.preview-logo.fluxer-logo-asset"
			/>
		);
	}
	return null;
}

function PreviewBackground({brand, scrim, highlight, onSelect}: SceneProps & {scrim: 'card' | 'phone'}) {
	return (
		<Hotspot
			target="sign_in_backdrop"
			highlight={highlight}
			onSelect={onSelect}
			className={styles.pageBackground}
			data-flx="app.setup.setup-branding-preview.preview-background.page-background"
		>
			{brand.backdropUrl != null && (
				<>
					<img
						className={styles.backdropImage}
						src={brand.backdropUrl}
						alt=""
						draggable={false}
						data-flx="app.setup.setup-branding-preview.preview-background.backdrop-image"
					/>
					<span
						className={clsx(styles.backdropScrim, scrim === 'phone' && styles.backdropScrimPhone)}
						data-flx="app.setup.setup-branding-preview.preview-background.backdrop-scrim"
					/>
				</>
			)}
		</Hotspot>
	);
}

function FormSkeleton({brand, highlight, onSelect}: SceneProps) {
	return (
		<div className={styles.formSkeleton} data-flx="app.setup.setup-branding-preview.form-skeleton.form-skeleton">
			<div className={styles.skeletonTitle} data-flx="app.setup.setup-branding-preview.form-skeleton.skeleton-title" />
			<div className={styles.skeletonLine} data-flx="app.setup.setup-branding-preview.form-skeleton.skeleton-line" />
			<div className={styles.skeletonInput} data-flx="app.setup.setup-branding-preview.form-skeleton.skeleton-input" />
			<div
				className={styles.skeletonInput}
				data-flx="app.setup.setup-branding-preview.form-skeleton.skeleton-input--2"
			/>
			<Hotspot
				target="theme_color"
				highlight={highlight}
				onSelect={onSelect}
				className={styles.skeletonButton}
				style={brand.accent ? {backgroundColor: brand.accent} : undefined}
				data-flx="app.setup.setup-branding-preview.form-skeleton.skeleton-button"
			>
				<span
					className={styles.skeletonButtonText}
					style={brand.textOnAccent ? {backgroundColor: brand.textOnAccent} : undefined}
					data-flx="app.setup.setup-branding-preview.form-skeleton.skeleton-button-text"
				/>
			</Hotspot>
		</div>
	);
}

function SignInPreview({brand, highlight, onSelect}: SceneProps) {
	const hasLogo = brand.logoUrl != null || brand.showBundledLogo;
	return (
		<div className={styles.computerFrame} data-flx="app.setup.setup-branding-preview.sign-in-preview.computer-frame">
			<div className={styles.browserChrome} data-flx="app.setup.setup-branding-preview.sign-in-preview.browser-chrome">
				<div className={styles.browserTabs} data-flx="app.setup.setup-branding-preview.sign-in-preview.browser-tabs">
					<div className={styles.browserTab} data-flx="app.setup.setup-branding-preview.sign-in-preview.browser-tab">
						<Hotspot
							target={brand.faviconTarget}
							highlightTargets={['favicon', 'favicon_dark', 'icon']}
							highlight={highlight}
							onSelect={onSelect}
							className={styles.favicon}
							data-flx="app.setup.setup-branding-preview.sign-in-preview.favicon"
						>
							{brand.faviconUrl != null ? (
								<img
									src={brand.faviconUrl}
									alt=""
									draggable={false}
									data-flx="app.setup.setup-branding-preview.sign-in-preview.img"
								/>
							) : (
								<FluxerLogoAsset
									aria-hidden="true"
									data-flx="app.setup.setup-branding-preview.sign-in-preview.fluxer-logo-asset"
								/>
							)}
						</Hotspot>
						<span
							className={styles.browserTabTitle}
							dir="auto"
							data-flx="app.setup.setup-branding-preview.sign-in-preview.browser-tab-title"
						>
							{brand.productName}
						</span>
						<XIcon
							className={styles.browserTabClose}
							weight="bold"
							data-flx="app.setup.setup-branding-preview.sign-in-preview.browser-tab-close"
						/>
					</div>
				</div>
				<div className={styles.addressBar} data-flx="app.setup.setup-branding-preview.sign-in-preview.address-bar">
					<LockSimpleIcon
						weight="bold"
						className={styles.addressIcon}
						data-flx="app.setup.setup-branding-preview.sign-in-preview.address-icon"
					/>
					<span dir="ltr" data-flx="app.setup.setup-branding-preview.sign-in-preview.span">
						{brand.host}
					</span>
				</div>
			</div>
			<div className={styles.computerPage} data-flx="app.setup.setup-branding-preview.sign-in-preview.computer-page">
				<PreviewBackground
					brand={brand}
					scrim="card"
					highlight={highlight}
					onSelect={onSelect}
					data-flx="app.setup.setup-branding-preview.sign-in-preview.preview-background"
				/>
				<div
					className={clsx(styles.card, hasLogo && styles.cardSplit)}
					data-flx="app.setup.setup-branding-preview.sign-in-preview.card"
				>
					<div className={styles.cardBrand} data-flx="app.setup.setup-branding-preview.sign-in-preview.card-brand">
						{hasLogo && (
							<Hotspot
								target="logo"
								highlightTargets={['logo', 'icon']}
								highlight={highlight}
								onSelect={onSelect}
								className={styles.cardLogoBox}
								data-flx="app.setup.setup-branding-preview.sign-in-preview.card-logo-box"
							>
								<PreviewLogo
									brand={brand}
									className={styles.cardLogo}
									data-flx="app.setup.setup-branding-preview.sign-in-preview.card-logo"
								/>
							</Hotspot>
						)}
						<Hotspot
							target="wordmark"
							highlight={highlight}
							onSelect={onSelect}
							className={styles.cardWordmarkBox}
							data-flx="app.setup.setup-branding-preview.sign-in-preview.card-wordmark-box"
						>
							<PreviewWordmark
								brand={brand}
								className={styles.cardWordmark}
								data-flx="app.setup.setup-branding-preview.sign-in-preview.card-wordmark"
							/>
						</Hotspot>
						<span
							className={styles.cardHost}
							dir="ltr"
							data-flx="app.setup.setup-branding-preview.sign-in-preview.card-host"
						>
							{brand.host}
						</span>
					</div>
					<div className={styles.cardForm} data-flx="app.setup.setup-branding-preview.sign-in-preview.card-form">
						<FormSkeleton
							brand={brand}
							highlight={highlight}
							onSelect={onSelect}
							data-flx="app.setup.setup-branding-preview.sign-in-preview.form-skeleton"
						/>
					</div>
				</div>
			</div>
		</div>
	);
}

function PhonePreview({brand, highlight, onSelect}: SceneProps) {
	return (
		<div className={styles.phoneFrame} data-flx="app.setup.setup-branding-preview.phone-preview.phone-frame">
			<Hotspot
				target="theme_color"
				highlight={highlight}
				onSelect={onSelect}
				className={styles.phoneToolbar}
				style={{backgroundColor: brand.toolbarColor ?? undefined, color: brand.toolbarText}}
				data-flx="app.setup.setup-branding-preview.phone-preview.phone-toolbar"
			>
				<span
					className={styles.phoneAddress}
					dir="ltr"
					data-flx="app.setup.setup-branding-preview.phone-preview.phone-address"
				>
					{brand.host}
				</span>
			</Hotspot>
			<div className={styles.phonePage} data-flx="app.setup.setup-branding-preview.phone-preview.phone-page">
				<PreviewBackground
					brand={brand}
					scrim="phone"
					highlight={highlight}
					onSelect={onSelect}
					data-flx="app.setup.setup-branding-preview.phone-preview.preview-background"
				/>
				<div className={styles.phoneContent} data-flx="app.setup.setup-branding-preview.phone-preview.phone-content">
					<div
						className={styles.phoneIdentity}
						data-flx="app.setup.setup-branding-preview.phone-preview.phone-identity"
					>
						{phoneShowsLogo(brand) && (
							<Hotspot
								target="logo"
								highlightTargets={['logo', 'icon']}
								highlight={highlight}
								onSelect={onSelect}
								className={styles.phoneLogoBox}
								data-flx="app.setup.setup-branding-preview.phone-preview.phone-logo-box"
							>
								<PreviewLogo
									brand={brand}
									className={styles.phoneLogo}
									data-flx="app.setup.setup-branding-preview.phone-preview.phone-logo"
								/>
							</Hotspot>
						)}
						<Hotspot
							target="wordmark"
							highlight={highlight}
							onSelect={onSelect}
							className={styles.phoneWordmarkBox}
							data-flx="app.setup.setup-branding-preview.phone-preview.phone-wordmark-box"
						>
							<PreviewWordmark
								brand={brand}
								className={styles.phoneWordmark}
								data-flx="app.setup.setup-branding-preview.phone-preview.phone-wordmark"
							/>
						</Hotspot>
					</div>
					<span
						className={styles.phoneHost}
						dir="ltr"
						data-flx="app.setup.setup-branding-preview.phone-preview.phone-host"
					>
						{brand.host}
					</span>
					<FormSkeleton
						brand={brand}
						highlight={highlight}
						onSelect={onSelect}
						data-flx="app.setup.setup-branding-preview.phone-preview.form-skeleton"
					/>
				</div>
			</div>
		</div>
	);
}

function TitlebarPreview({
	brand,
	platform,
	theme,
	label,
	highlight,
	onSelect,
}: SceneProps & {platform: DesktopPlatform; theme: 'dark' | 'light'; label: string}) {
	const {i18n} = useLingui();
	const rtl = getLocaleDirection(i18n.locale) === 'rtl';
	const wordmark = (
		<Hotspot
			target="wordmark"
			highlight={highlight}
			onSelect={onSelect}
			className={styles.titlebarBrand}
			data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar-brand"
		>
			<PreviewWordmark
				brand={brand}
				className={styles.titlebarWordmark}
				data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar-wordmark"
			/>
		</Hotspot>
	);
	return (
		<figure
			className={styles.titlebarFigure}
			data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar-figure"
		>
			<figcaption
				className={styles.titlebarLabel}
				data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar-label"
			>
				{label}
			</figcaption>
			<div
				className={clsx(styles.window, theme === 'light' ? styles.windowLight : styles.windowDark)}
				data-flx="app.setup.setup-branding-preview.titlebar-preview.window"
			>
				<div
					className={clsx(styles.titlebar, platform === 'macos' && styles.titlebarMacos)}
					data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar"
				>
					{platform === 'macos' ? (
						<>
							<span
								className={styles.trafficLights}
								data-flx="app.setup.setup-branding-preview.titlebar-preview.traffic-lights"
							>
								<span data-flx="app.setup.setup-branding-preview.titlebar-preview.span" />
								<span data-flx="app.setup.setup-branding-preview.titlebar-preview.span--2" />
								<span data-flx="app.setup.setup-branding-preview.titlebar-preview.span--3" />
							</span>
							{rtl && wordmark}
							<span
								className={styles.titlebarSpacer}
								data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar-spacer"
							/>
							{!rtl && wordmark}
						</>
					) : (
						<>
							{wordmark}
							<span
								className={styles.titlebarSpacer}
								data-flx="app.setup.setup-branding-preview.titlebar-preview.titlebar-spacer--2"
							/>
							<span
								className={styles.windowControls}
								data-flx="app.setup.setup-branding-preview.titlebar-preview.window-controls"
							>
								<MinusIcon weight="bold" data-flx="app.setup.setup-branding-preview.titlebar-preview.minus-icon" />
								<SquareIcon weight="bold" data-flx="app.setup.setup-branding-preview.titlebar-preview.square-icon" />
								<XIcon weight="bold" data-flx="app.setup.setup-branding-preview.titlebar-preview.x-icon" />
							</span>
						</>
					)}
				</div>
				<div className={styles.windowBody} data-flx="app.setup.setup-branding-preview.titlebar-preview.window-body">
					<span
						className={styles.windowRail}
						data-flx="app.setup.setup-branding-preview.titlebar-preview.window-rail"
					/>
					<span
						className={styles.windowSidebar}
						data-flx="app.setup.setup-branding-preview.titlebar-preview.window-sidebar"
					/>
					<span className={styles.windowMain} data-flx="app.setup.setup-branding-preview.titlebar-preview.window-main">
						<span
							className={styles.windowLine}
							data-flx="app.setup.setup-branding-preview.titlebar-preview.window-line"
						/>
						<span
							className={styles.windowLine}
							data-flx="app.setup.setup-branding-preview.titlebar-preview.window-line--2"
						/>
						<span
							className={styles.windowLineShort}
							data-flx="app.setup.setup-branding-preview.titlebar-preview.window-line-short"
						/>
					</span>
				</div>
			</div>
		</figure>
	);
}

function DesktopPreview({brand, highlight, onSelect}: SceneProps) {
	const {i18n} = useLingui();
	const [platform, setPlatform] = useState<DesktopPlatform>('controls');
	const platforms = useMemo(
		() => [
			{id: 'controls' as const, label: i18n._(WINDOWS_LINUX_DESCRIPTOR)},
			{id: 'macos' as const, label: i18n._(MACOS_DESCRIPTOR)},
		],
		[i18n],
	);
	return (
		<div className={styles.desktopScene} data-flx="app.setup.setup-branding-preview.desktop-preview.desktop-scene">
			<SegmentedTabs
				tabs={platforms}
				selectedTab={platform}
				onTabChange={setPlatform}
				ariaLabel={i18n._(TAB_DESKTOP_DESCRIPTOR)}
				semantics="radio"
				className={styles.platformTabs}
				data-flx="app.setup.setup-branding-preview.desktop-preview.platform-tabs"
			/>
			<div
				className={styles.windows}
				aria-hidden="true"
				data-flx="app.setup.setup-branding-preview.desktop-preview.windows"
			>
				{(['dark', 'light'] as const).map((theme) => (
					<TitlebarPreview
						key={theme}
						brand={brand}
						platform={platform}
						theme={theme}
						highlight={highlight}
						onSelect={onSelect}
						label={i18n._(theme === 'dark' ? DARK_THEME_DESCRIPTOR : LIGHT_THEME_DESCRIPTOR)}
						data-flx="app.setup.setup-branding-preview.desktop-preview.titlebar-preview"
					/>
				))}
			</div>
		</div>
	);
}

function InstancesPreview({brand, highlight, onSelect}: SceneProps) {
	return (
		<div className={styles.instanceStage} data-flx="app.setup.setup-branding-preview.instances-preview.instance-stage">
			<div className={styles.instanceRow} data-flx="app.setup.setup-branding-preview.instances-preview.instance-row">
				<Hotspot
					target="symbol"
					highlightTargets={['symbol', 'icon', 'logo']}
					highlight={highlight}
					onSelect={onSelect}
					className={styles.instanceMark}
					data-flx="app.setup.setup-branding-preview.instances-preview.instance-mark"
				>
					{brand.symbolUrl != null ? (
						<img
							src={brand.symbolUrl}
							alt=""
							draggable={false}
							data-flx="app.setup.setup-branding-preview.instances-preview.img"
						/>
					) : (
						<GlobeIcon weight="regular" data-flx="app.setup.setup-branding-preview.instances-preview.globe-icon" />
					)}
				</Hotspot>
				<span
					className={styles.instanceMeta}
					data-flx="app.setup.setup-branding-preview.instances-preview.instance-meta"
				>
					<span
						className={styles.instanceName}
						dir="auto"
						data-flx="app.setup.setup-branding-preview.instances-preview.instance-name"
					>
						{brand.productName}
					</span>
					<span
						className={styles.instanceHost}
						dir="ltr"
						data-flx="app.setup.setup-branding-preview.instances-preview.instance-host"
					>
						{brand.host}
					</span>
				</span>
			</div>
			{[0, 1].map((row) => (
				<div
					key={row}
					className={clsx(styles.instanceRow, styles.instanceRowOther)}
					data-flx="app.setup.setup-branding-preview.instances-preview.instance-row--2"
				>
					<span
						className={styles.instanceMark}
						data-flx="app.setup.setup-branding-preview.instances-preview.instance-mark"
					>
						<GlobeIcon weight="regular" data-flx="app.setup.setup-branding-preview.instances-preview.globe-icon--2" />
					</span>
					<span
						className={styles.instanceMeta}
						data-flx="app.setup.setup-branding-preview.instances-preview.instance-meta--2"
					>
						<span
							className={styles.instanceNameSkeleton}
							data-flx="app.setup.setup-branding-preview.instances-preview.instance-name-skeleton"
						/>
						<span
							className={styles.instanceHostSkeleton}
							data-flx="app.setup.setup-branding-preview.instances-preview.instance-host-skeleton"
						/>
					</span>
				</div>
			))}
		</div>
	);
}

const TAB_CAPTIONS = {
	sign_in: CAPTION_SIGN_IN_DESCRIPTOR,
	phone: CAPTION_PHONE_DESCRIPTOR,
	desktop: CAPTION_DESKTOP_DESCRIPTOR,
	instances: CAPTION_INSTANCES_DESCRIPTOR,
} as const;

export function SetupBrandingPreview({
	draft,
	tab,
	highlight,
	caption,
	onTabChange,
	onTargetSelect,
}: BrandingPreviewProps): React.ReactElement {
	const {i18n} = useLingui();
	const brand = useMemo(() => resolvePreviewBrand(draft), [draft]);
	const tabs = useMemo(
		() => [
			{id: 'sign_in' as const, label: i18n._(TAB_SIGN_IN_DESCRIPTOR)},
			{id: 'phone' as const, label: i18n._(TAB_PHONE_DESCRIPTOR)},
			{id: 'desktop' as const, label: i18n._(TAB_DESKTOP_DESCRIPTOR)},
			{id: 'instances' as const, label: i18n._(TAB_INSTANCES_DESCRIPTOR)},
		],
		[i18n],
	);
	const scene = {brand, highlight, onSelect: onTargetSelect};
	const tabsScrollerRef = useRef<HTMLDivElement>(null);
	useEffect(() => {
		tabsScrollerRef.current
			?.querySelector<HTMLElement>(`[role="radio"][aria-checked="true"]`)
			?.scrollIntoView({block: 'nearest', inline: 'nearest'});
	}, [tab]);
	const stageRef = useRef<HTMLDivElement>(null);
	useEffect(() => {
		const stage = stageRef.current;
		if (!stage) return;
		const keepFocus = (event: MouseEvent) => {
			if (event.target instanceof Element && event.target.closest('[role="radio"]')) return;
			event.preventDefault();
		};
		stage.addEventListener('mousedown', keepFocus);
		return () => stage.removeEventListener('mousedown', keepFocus);
	}, []);
	return (
		<div className={styles.preview} data-flx="app.setup.setup-branding-preview.preview">
			<div
				ref={tabsScrollerRef}
				className={styles.tabsScroller}
				data-flx="app.setup.setup-branding-preview.tabs-scroller"
			>
				<SegmentedTabs
					tabs={tabs}
					selectedTab={tab}
					onTabChange={onTabChange}
					ariaLabel={i18n._(PREVIEW_TABS_DESCRIPTOR)}
					semantics="radio"
					className={styles.tabs}
					data-flx="app.setup.setup-branding-preview.tabs"
				/>
			</div>
			<div ref={stageRef} className={styles.stage} data-flx="app.setup.setup-branding-preview.stage">
				<div
					className={styles.scene}
					aria-hidden={tab === 'desktop' ? undefined : true}
					data-flx="app.setup.setup-branding-preview.scene"
				>
					{tab === 'sign_in' && (
						<SignInPreview data-flx="app.setup.setup-branding-preview.sign-in-preview" {...scene} />
					)}
					{tab === 'phone' && <PhonePreview data-flx="app.setup.setup-branding-preview.phone-preview" {...scene} />}
					{tab === 'desktop' && (
						<DesktopPreview data-flx="app.setup.setup-branding-preview.desktop-preview" {...scene} />
					)}
					{tab === 'instances' && (
						<InstancesPreview data-flx="app.setup.setup-branding-preview.instances-preview" {...scene} />
					)}
				</div>
			</div>
			<div className={styles.caption} data-flx="app.setup.setup-branding-preview.caption">
				{caption ?? <p data-flx="app.setup.setup-branding-preview.p">{i18n._(TAB_CAPTIONS[tab])}</p>}
			</div>
		</div>
	);
}
