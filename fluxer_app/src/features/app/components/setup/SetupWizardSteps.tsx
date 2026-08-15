// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import type {SetupBrandingAssetKind} from '@app/features/app/components/setup/SetupWizardClient';
import {
	createRandomWelcomeRotationState,
	createWelcomeRotationState,
	WELCOME_ROTATION,
} from '@app/features/app/components/setup/SetupWizardWelcomeRotation';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {Button} from '@app/features/ui/button/Button';
import {ColorPickerField} from '@app/features/ui/components/form/ColorPickerField';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Spinner} from '@app/features/ui/components/Spinner';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {ThemeSelector} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeTabContent';
import {LanguageSelector} from '@app/features/user/components/modals/tabs/LanguageTab';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ImageIcon, TrashIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion, type Transition, useReducedMotion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

export type RegistrationMode = 'open' | 'approval' | 'closed';
export type PremiumMode = 'mirror' | 'everyone';

export interface BrandingAssetState {
	kind: SetupBrandingAssetKind;
	url: string | null;
	preview: string | null;
}

const WELCOME_AUTHED_TITLE_DESCRIPTOR = msg({
	message: 'Welcome to {productName}',
	comment: 'Setup wizard title shown once the operator is signed in as the instance admin.',
});
const WELCOME_AUTHED_BODY_DESCRIPTOR = msg({
	message:
		'You are signed in as the instance administrator. The next steps configure branding, registration, and core policy for everyone on this instance.',
	comment: 'Setup wizard body shown to the signed-in instance administrator.',
});
const WELCOME_UNAUTHED_TITLE_DESCRIPTOR = msg({
	message: 'Create the administrator account',
	comment: 'Setup wizard title prompting the operator to create the first account.',
});
const WELCOME_UNAUTHED_BODY_DESCRIPTOR = msg({
	message: 'This instance has no accounts yet. The first account you create automatically becomes the administrator.',
	comment: 'Setup wizard body prompting the operator to register the first account.',
});
const WELCOME_LANGUAGE_LABEL_DESCRIPTOR = msg({
	message: 'Language',
	comment: 'Label for the setup wizard language selector.',
});
const THEME_TITLE_DESCRIPTOR = msg({
	message: 'Choose a theme',
	comment: 'Setup wizard title for choosing the initial account theme.',
});
const THEME_BODY_DESCRIPTOR = msg({
	message: 'Pick the appearance this administrator account should start with.',
	comment: 'Setup wizard body for choosing the initial account theme.',
});
export const CREATE_ADMIN_ACCOUNT_DESCRIPTOR = msg({
	message: 'Create administrator account',
	comment: 'Button that opens the registration flow to create the first administrator account.',
});
const LOADING_TITLE_DESCRIPTOR = msg({
	message: 'Preparing setup',
	comment: 'Setup wizard title while the instance configuration loads.',
});
const LOADING_BODY_DESCRIPTOR = msg({
	message: 'Loading the current instance configuration.',
	comment: 'Setup wizard body while the instance configuration loads.',
});

const BRANDING_TITLE_DESCRIPTOR = msg({
	message: 'Make the instance yours',
	comment: 'Setup wizard branding step title.',
});
const BRANDING_BODY_DESCRIPTOR = msg({
	message: 'Set the public product name, theme color, and image assets that clients display.',
	comment: 'Setup wizard branding step body.',
});
const PRODUCT_NAME_LABEL_DESCRIPTOR = msg({
	message: 'Product name',
	comment: 'Label for the product name input in the setup wizard.',
});
const PRODUCT_NAME_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Fluxer',
	comment: 'Placeholder for the product name input in the setup wizard.',
});
const PRODUCT_NAME_ERROR_DESCRIPTOR = msg({
	message: 'Enter a product name between 1 and 80 characters.',
	comment: 'Validation error for the product name input in the setup wizard.',
});
const THEME_COLOR_LABEL_DESCRIPTOR = msg({
	message: 'Theme color',
	comment: 'Label for the theme color picker in the setup wizard.',
});
const BRANDING_ASSETS_LABEL_DESCRIPTOR = msg({
	message: 'Image assets',
	comment: 'Label for the branding image asset uploaders in the setup wizard.',
});
const UPLOAD_ASSET_DESCRIPTOR = msg({
	message: 'Upload',
	comment: 'Button that opens a file picker to upload a branding image.',
});
const REPLACE_ASSET_DESCRIPTOR = msg({
	message: 'Replace',
	comment: 'Button that replaces an already uploaded branding image.',
});
const CLEAR_ASSET_DESCRIPTOR = msg({
	message: 'Clear',
	comment: 'Button that removes an uploaded branding image.',
});
const ASSET_ICON_DESCRIPTOR = msg({
	message: 'Icon',
	comment: 'Label for the icon branding asset.',
});
const ASSET_SYMBOL_DESCRIPTOR = msg({
	message: 'Symbol',
	comment: 'Label for the symbol branding asset.',
});
const ASSET_LOGO_DESCRIPTOR = msg({
	message: 'Logo',
	comment: 'Label for the logo branding asset.',
});
const ASSET_WORDMARK_DESCRIPTOR = msg({
	message: 'Wordmark',
	comment: 'Label for the wordmark branding asset.',
});
const ASSET_FAVICON_DESCRIPTOR = msg({
	message: 'Favicon',
	comment: 'Label for the favicon branding asset.',
});
const ASSET_PREVIEW_ALT_DESCRIPTOR = msg({
	message: 'Branding asset preview',
	comment: 'Accessible label for a branding image preview in the setup wizard.',
});

const REGISTRATION_TITLE_DESCRIPTOR = msg({
	message: 'Who can join',
	comment: 'Setup wizard registration step title.',
});
const REGISTRATION_BODY_DESCRIPTOR = msg({
	message: 'Choose how new accounts are created on this instance.',
	comment: 'Setup wizard registration step body.',
});
const REGISTRATION_OPEN_NAME_DESCRIPTOR = msg({
	message: 'Open',
	comment: 'Registration mode option allowing anyone to register.',
});
const REGISTRATION_OPEN_DESC_DESCRIPTOR = msg({
	message: 'Anyone can create an account.',
	comment: 'Description for the open registration mode.',
});
const REGISTRATION_APPROVAL_NAME_DESCRIPTOR = msg({
	message: 'Approval required',
	comment: 'Registration mode option requiring admin approval.',
});
const REGISTRATION_APPROVAL_DESC_DESCRIPTOR = msg({
	message: 'Anyone can request an account, but an administrator must approve it.',
	comment: 'Description for the approval registration mode.',
});
const REGISTRATION_CLOSED_NAME_DESCRIPTOR = msg({
	message: 'Closed',
	comment: 'Registration mode option that disables public registration.',
});
const REGISTRATION_CLOSED_DESC_DESCRIPTOR = msg({
	message: 'Public registration is closed.',
	comment: 'Description for the closed registration mode.',
});

const COMMUNITY_TITLE_DESCRIPTOR = msg({
	message: 'Choose how your instance works',
	comment: 'Setup wizard community model step title.',
});
const COMMUNITY_BODY_DESCRIPTOR = msg({
	message: 'These options change how people use the instance. Some can only be chosen during setup.',
	comment: 'Setup wizard community model step body.',
});
const SINGLE_COMMUNITY_LABEL_DESCRIPTOR = msg({
	message: 'Single community mode',
	comment: 'Label for the single community toggle in the setup wizard.',
});
const SINGLE_COMMUNITY_DESC_DESCRIPTOR = msg({
	message: 'Run the instance as one community instead of many. This can only be enabled now, during setup.',
	comment: 'Description for the single community toggle in the setup wizard.',
});
const SINGLE_COMMUNITY_NAME_LABEL_DESCRIPTOR = msg({
	message: 'Community name',
	comment: 'Label for the single community name input in the setup wizard.',
});
const SINGLE_COMMUNITY_NAME_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'My community',
	comment: 'Placeholder for the single community name input in the setup wizard.',
});
const SINGLE_COMMUNITY_NAME_ERROR_DESCRIPTOR = msg({
	message: 'Enter a community name between 1 and 100 characters.',
	comment: 'Validation error for the single community name input in the setup wizard.',
});
const DISABLE_DM_LABEL_DESCRIPTOR = msg({
	message: 'Disable direct messages and friend requests',
	comment: 'Label for the toggle that disables direct messages and friend requests.',
});
const DISABLE_DM_DESC_DESCRIPTOR = msg({
	message: 'Turn off private messaging and friends across the instance. This can be reversed only once later.',
	comment: 'Description for the toggle that disables direct messages and friend requests.',
});

const MEDIA_EXPIRY_TITLE_DESCRIPTOR = msg({
	message: 'Attachment expiry',
	comment: 'Setup wizard media expiry step title.',
});
const MEDIA_EXPIRY_BODY_DESCRIPTOR = msg({
	message: 'Choose whether uploaded attachments should expire automatically based on size.',
	comment: 'Setup wizard media expiry step body.',
});
const MEDIA_EXPIRY_ENABLE_LABEL_DESCRIPTOR = msg({
	message: 'Enable attachment expiry',
	comment: 'Label for enabling attachment expiry during setup.',
});
const MEDIA_EXPIRY_ENABLE_DESC_DESCRIPTOR = msg({
	message: 'Disabled attachments remain available until manually removed.',
	comment: 'Description for the attachment expiry setup switch.',
});
const MEDIA_MIN_SIZE_LABEL_DESCRIPTOR = msg({
	message: 'Small file threshold (MB)',
	comment: 'Label for attachment decay minimum size threshold.',
});
const MEDIA_MAX_SIZE_LABEL_DESCRIPTOR = msg({
	message: 'Large file threshold (MB)',
	comment: 'Label for attachment decay maximum size threshold.',
});
const MEDIA_MAX_ELIGIBLE_SIZE_LABEL_DESCRIPTOR = msg({
	message: 'Maximum eligible size (MB)',
	comment: 'Label for attachment decay maximum eligible size.',
});
const MEDIA_MIN_LIFETIME_LABEL_DESCRIPTOR = msg({
	message: 'Minimum lifetime (days)',
	comment: 'Label for attachment decay minimum lifetime.',
});
const MEDIA_MAX_LIFETIME_LABEL_DESCRIPTOR = msg({
	message: 'Maximum lifetime (days)',
	comment: 'Label for attachment decay maximum lifetime.',
});
const MEDIA_CURVE_LABEL_DESCRIPTOR = msg({
	message: 'Expiry curve',
	comment: 'Label for attachment decay curve.',
});
const MEDIA_RENEW_THRESHOLD_LABEL_DESCRIPTOR = msg({
	message: 'Renew threshold (days)',
	comment: 'Label for attachment decay renewal threshold.',
});
const MEDIA_RENEW_WINDOW_LABEL_DESCRIPTOR = msg({
	message: 'Renew window (days)',
	comment: 'Label for attachment decay renewal window.',
});

const SERVICES_TITLE_DESCRIPTOR = msg({
	message: 'Optional services',
	comment: 'Setup wizard optional services step title.',
});
const SERVICES_BODY_DESCRIPTOR = msg({
	message: 'Enable the integrations whose credentials you have configured. Only available services are shown.',
	comment: 'Setup wizard optional services step body.',
});
const SERVICES_NONE_DESCRIPTOR = msg({
	message: 'No optional services are available on this instance. You can skip this step.',
	comment: 'Message shown when no optional services are available in the setup wizard.',
});
const SERVICE_GIF_LABEL_DESCRIPTOR = msg({
	message: 'Klipy GIFs',
	comment: 'Label for the Klipy GIF service toggle in the setup wizard.',
});
const SERVICE_GIF_DESC_DESCRIPTOR = msg({
	message: 'Let people search and send GIFs powered by Klipy.',
	comment: 'Description for the Klipy GIF service toggle in the setup wizard.',
});
const SERVICE_YOUTUBE_LABEL_DESCRIPTOR = msg({
	message: 'YouTube enrichment',
	comment: 'Label for the YouTube enrichment service toggle in the setup wizard.',
});
const SERVICE_YOUTUBE_DESC_DESCRIPTOR = msg({
	message: 'Show rich previews for YouTube links.',
	comment: 'Description for the YouTube enrichment service toggle in the setup wizard.',
});
const SERVICE_BLUESKY_LABEL_DESCRIPTOR = msg({
	message: 'Bluesky connections',
	comment: 'Label for the Bluesky connections service toggle in the setup wizard.',
});
const SERVICE_BLUESKY_DESC_DESCRIPTOR = msg({
	message: 'Let people link their Bluesky account to their profile.',
	comment: 'Description for the Bluesky connections service toggle in the setup wizard.',
});

const PREMIUM_TITLE_DESCRIPTOR = msg({
	message: 'Choose the premium model',
	comment: 'Setup wizard premium model step title.',
});
const PREMIUM_BODY_DESCRIPTOR = msg({
	message: 'Decide how premium limits apply to people on this instance.',
	comment: 'Setup wizard premium model step body.',
});
const PREMIUM_MIRROR_NAME_DESCRIPTOR = msg({
	message: 'Mirror tiers',
	comment: 'Premium model option that mirrors free and premium tiers.',
});
const PREMIUM_MIRROR_DESC_DESCRIPTOR = msg({
	message: 'Keep Free and Premium tiers. You can customize the tiers later.',
	comment: 'Description for the mirror premium model.',
});
const PREMIUM_EVERYONE_NAME_DESCRIPTOR = msg({
	message: 'Everyone is premium',
	comment: 'Premium model option that gives everyone the highest limits.',
});
const PREMIUM_EVERYONE_DESC_DESCRIPTOR = msg({
	message: 'Give everyone on this instance the highest premium limits.',
	comment: 'Description for the everyone-is-premium model.',
});

const FINISH_TITLE_DESCRIPTOR = msg({
	message: 'Finish setup',
	comment: 'Setup wizard finish step title.',
});
const FINISH_BODY_DESCRIPTOR = msg({
	message: 'Review your choices below, then finish. This saves the configuration and opens the instance.',
	comment: 'Setup wizard finish step body.',
});
const SUMMARY_PRODUCT_NAME_DESCRIPTOR = msg({
	message: 'Product name',
	comment: 'Summary row label for the product name in the setup wizard.',
});
const SUMMARY_REGISTRATION_DESCRIPTOR = msg({
	message: 'Registration',
	comment: 'Summary row label for the registration mode in the setup wizard.',
});
const SUMMARY_SINGLE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Single community',
	comment: 'Summary row label for the single community choice in the setup wizard.',
});
const SUMMARY_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Direct messages',
	comment: 'Summary row label for the direct messages choice in the setup wizard.',
});
const SUMMARY_ATTACHMENT_EXPIRY_DESCRIPTOR = msg({
	message: 'Attachment expiry',
	comment: 'Summary row label for the attachment expiry choice in the setup wizard.',
});
const SUMMARY_PREMIUM_DESCRIPTOR = msg({
	message: 'Premium model',
	comment: 'Summary row label for the premium model in the setup wizard.',
});
const SUMMARY_ON_DESCRIPTOR = msg({
	message: 'Enabled',
	comment: 'Summary value when a setup option is enabled.',
});
const SUMMARY_OFF_DESCRIPTOR = msg({
	message: 'Disabled',
	comment: 'Summary value when a setup option is disabled.',
});

const ASSET_DESCRIPTORS: Record<SetupBrandingAssetKind, MessageDescriptor> = {
	icon: ASSET_ICON_DESCRIPTOR,
	symbol: ASSET_SYMBOL_DESCRIPTOR,
	logo: ASSET_LOGO_DESCRIPTOR,
	wordmark: ASSET_WORDMARK_DESCRIPTOR,
	favicon: ASSET_FAVICON_DESCRIPTOR,
};

const WELCOME_WORD_INLINE_PADDING_PX = 8;
const SETUP_LANGUAGE_MENU_MAX_HEIGHT = 220;

interface StepHeaderProps {
	title: string;
	body: string;
}

const StepHeader: React.FC<StepHeaderProps> = ({title, body}) => (
	<div className={styles.stepHeader} data-flx="app.self-hosted-setup-wizard-gate.step-header">
		<h2 className={styles.title} data-flx="app.self-hosted-setup-wizard-gate.title">
			{title}
		</h2>
		<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.body">
			{body}
		</p>
	</div>
);

export const WelcomeStep = observer(
	({
		productName,
		isAuthenticated,
		initialFocusRef,
	}: {
		productName: string;
		isAuthenticated: boolean;
		initialFocusRef?: React.Ref<HTMLElement>;
	}) => {
		const {i18n} = useLingui();
		const prefersReducedMotion = useReducedMotion();
		const currentLocale = LocaleUtils.getCurrentOrDetectedLocale();
		const welcomeFrameRef = useRef<HTMLDivElement | null>(null);
		const welcomeMeasureRef = useRef<HTMLSpanElement | null>(null);
		const [welcomeScale, setWelcomeScale] = useState(1);
		const [welcomeRotation, setWelcomeRotation] = useState(() => createWelcomeRotationState(currentLocale));
		const measureWelcomeScale = useCallback(() => {
			const frame = welcomeFrameRef.current;
			const measure = welcomeMeasureRef.current;
			if (!frame || !measure) return;
			const availableWidth = Math.max(0, frame.clientWidth - WELCOME_WORD_INLINE_PADDING_PX);
			const measuredWidth = Math.max(measure.scrollWidth, measure.getBoundingClientRect().width);
			const nextScale = measuredWidth > 0 && availableWidth > 0 ? Math.min(1, availableWidth / measuredWidth) : 1;
			setWelcomeScale((currentScale) => (Math.abs(currentScale - nextScale) > 0.005 ? nextScale : currentScale));
		}, []);
		useLayoutEffect(() => {
			measureWelcomeScale();
			const frame = welcomeFrameRef.current;
			if (!frame) return undefined;
			const ownerWindow = frame.ownerDocument.defaultView;
			const resizeObserver =
				typeof ownerWindow?.ResizeObserver === 'function' ? new ownerWindow.ResizeObserver(measureWelcomeScale) : null;
			resizeObserver?.observe(frame);
			ownerWindow?.addEventListener('resize', measureWelcomeScale);
			void frame.ownerDocument.fonts?.ready.then(measureWelcomeScale);
			return () => {
				resizeObserver?.disconnect();
				ownerWindow?.removeEventListener('resize', measureWelcomeScale);
			};
		}, [measureWelcomeScale, welcomeRotation]);
		useEffect(() => {
			setWelcomeRotation(createWelcomeRotationState(currentLocale));
		}, [currentLocale]);
		useEffect(() => {
			const interval = window.setInterval(() => {
				setWelcomeRotation((rotation) => {
					if (rotation.position + 1 < rotation.order.length) {
						return {
							...rotation,
							position: rotation.position + 1,
						};
					}
					return createRandomWelcomeRotationState(rotation.order[rotation.position]);
				});
			}, 1800);
			return () => window.clearInterval(interval);
		}, []);
		const welcome = WELCOME_ROTATION[welcomeRotation.order[welcomeRotation.position] ?? 0];
		const motionState = prefersReducedMotion
			? {
					initial: {opacity: 0, scale: welcomeScale},
					animate: {opacity: 1, scale: welcomeScale},
					exit: {opacity: 0, scale: welcomeScale},
				}
			: {
					initial: {opacity: 0, y: 14, scale: welcomeScale * 0.98},
					animate: {opacity: 1, y: 0, scale: welcomeScale},
					exit: {opacity: 0, y: -14, scale: welcomeScale * 0.98},
				};
		return (
			<section
				ref={initialFocusRef}
				className={styles.step}
				tabIndex={-1}
				data-flx="app.self-hosted-setup-wizard-gate.welcome-step"
			>
				<div className={styles.welcomeHero} data-flx="app.self-hosted-setup-wizard-gate.welcome-hero">
					<div
						ref={welcomeFrameRef}
						className={styles.welcomeWordFrame}
						data-flx="app.self-hosted-setup-wizard-gate.welcome-word-frame"
					>
						<span
							ref={welcomeMeasureRef}
							className={styles.welcomeWordMeasure}
							aria-hidden="true"
							data-flx="app.self-hosted-setup-wizard-gate.welcome-word-measure"
						>
							{welcome.text}
						</span>
						<AnimatePresence mode="wait" initial={false}>
							<motion.h2
								key={welcome.code}
								className={styles.welcomeWord}
								initial={motionState.initial}
								animate={motionState.animate}
								exit={motionState.exit}
								transition={{duration: prefersReducedMotion ? 0.22 : 0.42, ease: [0.22, 1, 0.36, 1]}}
								data-flx="app.self-hosted-setup-wizard-gate.welcome-word"
							>
								{welcome.text}
							</motion.h2>
						</AnimatePresence>
					</div>
					{isAuthenticated ? (
						<>
							<h3 className={styles.welcomeTitle} data-flx="app.self-hosted-setup-wizard-gate.welcome-title">
								{i18n._(WELCOME_AUTHED_TITLE_DESCRIPTOR, {productName})}
							</h3>
							<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.welcome-body">
								{i18n._(WELCOME_AUTHED_BODY_DESCRIPTOR)}
							</p>
						</>
					) : null}
				</div>
				<div className={styles.localeBlock} data-flx="app.self-hosted-setup-wizard-gate.locale-block">
					<div className={styles.fieldLabel} data-flx="app.self-hosted-setup-wizard-gate.locale-label">
						{i18n._(WELCOME_LANGUAGE_LABEL_DESCRIPTOR)}
					</div>
					<LanguageSelector
						value={currentLocale}
						onChange={LocaleUtils.setLocalLocale}
						openMenuOnFocus={false}
						maxMenuHeight={SETUP_LANGUAGE_MENU_MAX_HEIGHT}
						menuPlacement="bottom"
						className={styles.localeSelector}
					/>
				</div>
			</section>
		);
	},
);

export const ThemeStep = observer(
	({theme, onThemeChange}: {theme: ThemeType; onThemeChange: (theme: ThemeType) => void}) => {
		const {i18n} = useLingui();
		const themeLabel = i18n._(THEME_TITLE_DESCRIPTOR);
		return (
			<section className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.theme-step">
				<StepHeader title={themeLabel} body={i18n._(THEME_BODY_DESCRIPTOR)} />
				<div className={styles.themeBlock} data-flx="app.self-hosted-setup-wizard-gate.theme-block">
					<ThemeSelector
						value={theme}
						onChange={onThemeChange}
						ariaLabel={themeLabel}
						className={styles.themeButtonGroup}
					/>
				</div>
			</section>
		);
	},
);

export const AdminIntroStep = observer(() => {
	const {i18n} = useLingui();
	return (
		<section className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.admin-intro-step">
			<StepHeader title={i18n._(WELCOME_UNAUTHED_TITLE_DESCRIPTOR)} body={i18n._(WELCOME_UNAUTHED_BODY_DESCRIPTOR)} />
		</section>
	);
});

export const AdminAccountStep = observer(({theme}: {theme: ThemeType}) => {
	const {i18n} = useLingui();
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.admin-account-step">
			<div className={styles.accountSetupForm} data-flx="app.self-hosted-setup-wizard-gate.admin-form">
				<AuthRegisterFormCore
					fields={{
						showEmail: true,
						showPassword: true,
						showPasswordConfirmation: true,
						showUsernameValidation: true,
					}}
					submitLabel={i18n._(CREATE_ADMIN_ACCOUNT_DESCRIPTOR)}
					redirectPath=""
					theme={theme}
					showLegalConsent={false}
					data-flx="app.self-hosted-setup-wizard-gate.admin-register-form"
				/>
			</div>
		</section>
	);
});

export const LoadingStep = observer(() => {
	const {i18n} = useLingui();
	return (
		<section className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.loading-step">
			<Spinner size="large" data-flx="app.self-hosted-setup-wizard-gate.loading-spinner" />
			<StepHeader title={i18n._(LOADING_TITLE_DESCRIPTOR)} body={i18n._(LOADING_BODY_DESCRIPTOR)} />
		</section>
	);
});

interface BrandingAssetRowProps {
	asset: BrandingAssetState;
	disabled: boolean;
	onUpload: (kind: SetupBrandingAssetKind) => void;
	onClear: (kind: SetupBrandingAssetKind) => void;
}

const BrandingAssetRow = observer(({asset, disabled, onUpload, onClear}: BrandingAssetRowProps) => {
	const {i18n} = useLingui();
	const previewSrc = asset.preview ?? asset.url;
	const hasImage = Boolean(previewSrc);
	return (
		<div className={styles.assetRow} data-flx="app.self-hosted-setup-wizard-gate.asset-row">
			<div className={styles.assetPreview} data-flx="app.self-hosted-setup-wizard-gate.asset-preview">
				{previewSrc ? (
					<img
						src={previewSrc}
						alt={i18n._(ASSET_PREVIEW_ALT_DESCRIPTOR)}
						className={styles.assetImage}
						data-flx="app.self-hosted-setup-wizard-gate.asset-image"
					/>
				) : (
					<ImageIcon size={22} weight="bold" data-flx="app.self-hosted-setup-wizard-gate.asset-placeholder" />
				)}
			</div>
			<div className={styles.assetLabel} data-flx="app.self-hosted-setup-wizard-gate.asset-label">
				{i18n._(ASSET_DESCRIPTORS[asset.kind])}
			</div>
			<div className={styles.assetActions} data-flx="app.self-hosted-setup-wizard-gate.asset-actions">
				<Button
					variant="secondary"
					small
					disabled={disabled}
					leftIcon={<UploadSimpleIcon size={16} weight="bold" />}
					onClick={() => onUpload(asset.kind)}
					data-flx="app.self-hosted-setup-wizard-gate.asset-upload-button"
				>
					{hasImage ? i18n._(REPLACE_ASSET_DESCRIPTOR) : i18n._(UPLOAD_ASSET_DESCRIPTOR)}
				</Button>
				{hasImage && (
					<Button
						variant="secondary"
						small
						square
						icon={<TrashIcon size={16} weight="bold" />}
						aria-label={i18n._(CLEAR_ASSET_DESCRIPTOR)}
						disabled={disabled}
						onClick={() => onClear(asset.kind)}
						data-flx="app.self-hosted-setup-wizard-gate.asset-clear-button"
					/>
				)}
			</div>
		</div>
	);
});

export const BrandingStep = observer(
	({
		productName,
		productNameError,
		themeColor,
		assets,
		disabled,
		onProductNameChange,
		onThemeColorChange,
		onUploadAsset,
		onClearAsset,
	}: {
		productName: string;
		productNameError: boolean;
		themeColor: number;
		assets: ReadonlyArray<BrandingAssetState>;
		disabled: boolean;
		onProductNameChange: (value: string) => void;
		onThemeColorChange: (value: number) => void;
		onUploadAsset: (kind: SetupBrandingAssetKind) => void;
		onClearAsset: (kind: SetupBrandingAssetKind) => void;
	}) => {
		const {i18n} = useLingui();
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.branding-step">
				<StepHeader title={i18n._(BRANDING_TITLE_DESCRIPTOR)} body={i18n._(BRANDING_BODY_DESCRIPTOR)} />
				<Input
					label={i18n._(PRODUCT_NAME_LABEL_DESCRIPTOR)}
					value={productName}
					onChange={(event) => onProductNameChange(event.target.value)}
					placeholder={i18n._(PRODUCT_NAME_PLACEHOLDER_DESCRIPTOR)}
					maxLength={80}
					disabled={disabled}
					error={productNameError ? i18n._(PRODUCT_NAME_ERROR_DESCRIPTOR) : undefined}
					data-step-focus="true"
					data-flx="app.self-hosted-setup-wizard-gate.product-name-input"
				/>
				<ColorPickerField
					label={i18n._(THEME_COLOR_LABEL_DESCRIPTOR)}
					value={themeColor}
					onChange={onThemeColorChange}
					disabled={disabled}
					data-flx="app.self-hosted-setup-wizard-gate.theme-color-field"
				/>
				<div className={styles.assetSection} data-flx="app.self-hosted-setup-wizard-gate.asset-section">
					<div className={styles.fieldLabel} data-flx="app.self-hosted-setup-wizard-gate.asset-section-label">
						{i18n._(BRANDING_ASSETS_LABEL_DESCRIPTOR)}
					</div>
					{assets.map((asset) => (
						<BrandingAssetRow
							key={asset.kind}
							asset={asset}
							disabled={disabled}
							onUpload={onUploadAsset}
							onClear={onClearAsset}
						/>
					))}
				</div>
			</section>
		);
	},
);

export const RegistrationStep = observer(
	({
		mode,
		disabled,
		onChange,
	}: {
		mode: RegistrationMode;
		disabled: boolean;
		onChange: (mode: RegistrationMode) => void;
	}) => {
		const {i18n} = useLingui();
		const options: ReadonlyArray<RadioOption<RegistrationMode>> = [
			{
				value: 'open',
				name: i18n._(REGISTRATION_OPEN_NAME_DESCRIPTOR),
				desc: i18n._(REGISTRATION_OPEN_DESC_DESCRIPTOR),
			},
			{
				value: 'approval',
				name: i18n._(REGISTRATION_APPROVAL_NAME_DESCRIPTOR),
				desc: i18n._(REGISTRATION_APPROVAL_DESC_DESCRIPTOR),
			},
			{
				value: 'closed',
				name: i18n._(REGISTRATION_CLOSED_NAME_DESCRIPTOR),
				desc: i18n._(REGISTRATION_CLOSED_DESC_DESCRIPTOR),
			},
		];
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.registration-step">
				<StepHeader title={i18n._(REGISTRATION_TITLE_DESCRIPTOR)} body={i18n._(REGISTRATION_BODY_DESCRIPTOR)} />
				<RadioGroup
					options={options}
					value={mode}
					onChange={onChange}
					disabled={disabled}
					aria-label={i18n._(REGISTRATION_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.registration-radio-group"
				/>
			</section>
		);
	},
);

export const CommunityStep = observer(
	({
		singleCommunityEnabled,
		singleCommunityName,
		singleCommunityNameError,
		directMessagesDisabled,
		disabled,
		onToggleSingleCommunity,
		onSingleCommunityNameChange,
		onToggleDirectMessages,
	}: {
		singleCommunityEnabled: boolean;
		singleCommunityName: string;
		singleCommunityNameError: boolean;
		directMessagesDisabled: boolean;
		disabled: boolean;
		onToggleSingleCommunity: (value: boolean) => void;
		onSingleCommunityNameChange: (value: string) => void;
		onToggleDirectMessages: (value: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.community-step">
				<StepHeader title={i18n._(COMMUNITY_TITLE_DESCRIPTOR)} body={i18n._(COMMUNITY_BODY_DESCRIPTOR)} />
				<Switch
					label={i18n._(SINGLE_COMMUNITY_LABEL_DESCRIPTOR)}
					description={i18n._(SINGLE_COMMUNITY_DESC_DESCRIPTOR)}
					value={singleCommunityEnabled}
					onChange={onToggleSingleCommunity}
					disabled={disabled}
					data-flx="app.self-hosted-setup-wizard-gate.single-community-switch"
				/>
				{singleCommunityEnabled && (
					<Input
						label={i18n._(SINGLE_COMMUNITY_NAME_LABEL_DESCRIPTOR)}
						value={singleCommunityName}
						onChange={(event) => onSingleCommunityNameChange(event.target.value)}
						placeholder={i18n._(SINGLE_COMMUNITY_NAME_PLACEHOLDER_DESCRIPTOR)}
						maxLength={100}
						disabled={disabled}
						error={singleCommunityNameError ? i18n._(SINGLE_COMMUNITY_NAME_ERROR_DESCRIPTOR) : undefined}
						data-flx="app.self-hosted-setup-wizard-gate.single-community-name-input"
					/>
				)}
				<Switch
					label={i18n._(DISABLE_DM_LABEL_DESCRIPTOR)}
					description={i18n._(DISABLE_DM_DESC_DESCRIPTOR)}
					value={directMessagesDisabled}
					onChange={onToggleDirectMessages}
					disabled={disabled}
					data-flx="app.self-hosted-setup-wizard-gate.disable-dm-switch"
				/>
			</section>
		);
	},
);

export interface ServiceAvailability {
	gif: boolean;
	youtube: boolean;
	bluesky: boolean;
}

export interface ServiceSelection {
	gif: boolean;
	youtube: boolean;
	bluesky: boolean;
}

export interface MediaExpiryDraft {
	enabled: boolean;
	minSizeMb: string;
	maxSizeMb: string;
	maxEligibleSizeMb: string;
	minLifetimeDays: string;
	maxLifetimeDays: string;
	curve: string;
	renewThresholdDays: string;
	renewWindowDays: string;
}

export type IntegrationStepKind = 'gif' | 'youtube' | 'captcha' | 'email' | 'bluesky';
export type IntegrationSetupMode = 'later' | 'configure';
export type CaptchaProvider = 'hcaptcha' | 'turnstile';

export interface ServiceIntegrationDraft {
	gifMode: IntegrationSetupMode;
	klipyApiKey: string;
	youtubeMode: IntegrationSetupMode;
	youtubeApiKey: string;
	captchaMode: IntegrationSetupMode;
	captchaProvider: CaptchaProvider;
	hcaptchaSiteKey: string;
	hcaptchaSecretKey: string;
	turnstileSiteKey: string;
	turnstileSecretKey: string;
	emailMode: IntegrationSetupMode;
	emailEnabled: boolean;
	emailFromEmail: string;
	emailFromName: string;
	smtpHost: string;
	smtpPort: string;
	smtpUsername: string;
	smtpPassword: string;
	smtpSecure: boolean;
	blueskyMode: IntegrationSetupMode;
	blueskyEnabled: boolean;
	blueskyClientName: string;
	blueskyClientUri: string;
	blueskyLogoUri: string;
	blueskyTosUri: string;
	blueskyPolicyUri: string;
	blueskyKeyId: string;
	blueskyPrivateKey: string;
}

const INTEGRATION_LATER_NAME_DESCRIPTOR = msg({
	message: 'Set up later',
	comment: 'Option for deferring an optional setup wizard integration.',
});
const INTEGRATION_LATER_DESC_DESCRIPTOR = msg({
	message: 'Skip this now and configure it from the admin panel.',
	comment: 'Description for deferring an optional setup wizard integration.',
});
const INTEGRATION_CONFIGURE_NAME_DESCRIPTOR = msg({
	message: 'Configure now',
	comment: 'Option for configuring an optional setup wizard integration.',
});
const INTEGRATION_CONFIGURE_DESC_DESCRIPTOR = msg({
	message: 'Add the credentials during setup.',
	comment: 'Description for configuring an optional setup wizard integration now.',
});
const GIF_SETUP_TITLE_DESCRIPTOR = msg({
	message: 'GIF search',
	comment: 'Setup wizard title for GIF integration credentials.',
});
const GIF_SETUP_BODY_DESCRIPTOR = msg({
	message: 'Add a Klipy API key to enable GIF search at runtime.',
	comment: 'Setup wizard body for GIF integration credentials.',
});
const YOUTUBE_SETUP_TITLE_DESCRIPTOR = msg({
	message: 'YouTube previews',
	comment: 'Setup wizard title for YouTube integration credentials.',
});
const YOUTUBE_SETUP_BODY_DESCRIPTOR = msg({
	message: 'Add a YouTube Data API key to enrich YouTube links.',
	comment: 'Setup wizard body for YouTube integration credentials.',
});
const CAPTCHA_SETUP_TITLE_DESCRIPTOR = msg({
	message: 'Bot protection',
	comment: 'Setup wizard title for CAPTCHA integration credentials.',
});
const CAPTCHA_SETUP_BODY_DESCRIPTOR = msg({
	message: 'Choose hCaptcha or Cloudflare Turnstile for signup challenges.',
	comment: 'Setup wizard body for CAPTCHA integration credentials.',
});
const EMAIL_SETUP_TITLE_DESCRIPTOR = msg({
	message: 'Email delivery',
	comment: 'Setup wizard title for SMTP integration credentials.',
});
const EMAIL_SETUP_BODY_DESCRIPTOR = msg({
	message: 'Add SMTP credentials so the instance can send verification and notification email.',
	comment: 'Setup wizard body for SMTP integration credentials.',
});
const BLUESKY_SETUP_TITLE_DESCRIPTOR = msg({
	message: 'Bluesky OAuth',
	comment: 'Setup wizard title for Bluesky integration credentials.',
});
const BLUESKY_SETUP_BODY_DESCRIPTOR = msg({
	message: 'Add Bluesky OAuth client metadata and a signing key.',
	comment: 'Setup wizard body for Bluesky integration credentials.',
});
const PROVIDER_LABEL_DESCRIPTOR = msg({
	message: 'Provider',
	comment: 'Label for an integration provider choice.',
});
const HCAPTCHA_NAME_DESCRIPTOR = msg({message: 'hCaptcha', comment: 'hCaptcha provider option.'});
const TURNSTILE_NAME_DESCRIPTOR = msg({
	message: 'Cloudflare Turnstile',
	comment: 'Cloudflare Turnstile provider option.',
});
const API_KEY_LABEL_DESCRIPTOR = msg({message: 'API key', comment: 'Label for an integration API key input.'});
const SITE_KEY_LABEL_DESCRIPTOR = msg({message: 'Site key', comment: 'Label for a CAPTCHA site key input.'});
const SECRET_KEY_LABEL_DESCRIPTOR = msg({message: 'Secret key', comment: 'Label for a CAPTCHA secret key input.'});
const ENABLE_EMAIL_LABEL_DESCRIPTOR = msg({
	message: 'Enable email delivery',
	comment: 'Label for enabling SMTP email delivery in setup.',
});
const EMAIL_FROM_EMAIL_LABEL_DESCRIPTOR = msg({
	message: 'From email',
	comment: 'Label for SMTP from email input.',
});
const EMAIL_FROM_NAME_LABEL_DESCRIPTOR = msg({
	message: 'From name',
	comment: 'Label for SMTP from name input.',
});
const SMTP_HOST_LABEL_DESCRIPTOR = msg({message: 'SMTP host', comment: 'Label for SMTP host input.'});
const SMTP_PORT_LABEL_DESCRIPTOR = msg({message: 'Port', comment: 'Label for SMTP port input.'});
const SMTP_USERNAME_LABEL_DESCRIPTOR = msg({message: 'Username', comment: 'Label for SMTP username input.'});
const SMTP_PASSWORD_LABEL_DESCRIPTOR = msg({message: 'Password', comment: 'Label for SMTP password input.'});
const SMTP_SECURE_LABEL_DESCRIPTOR = msg({
	message: 'Use TLS',
	comment: 'Label for SMTP TLS toggle.',
});
const SMTP_TEST_DESCRIPTOR = msg({
	message: 'Test SMTP',
	comment: 'Button that validates SMTP credentials.',
});
const SMTP_TEST_OK_DESCRIPTOR = msg({
	message: 'SMTP connection verified.',
	comment: 'Success message after validating SMTP credentials.',
});
const BLUESKY_ENABLED_LABEL_DESCRIPTOR = msg({
	message: 'Enable Bluesky OAuth',
	comment: 'Label for enabling Bluesky OAuth in setup.',
});
const BLUESKY_CLIENT_NAME_LABEL_DESCRIPTOR = msg({
	message: 'Client name',
	comment: 'Label for Bluesky OAuth client name input.',
});
const BLUESKY_CLIENT_URI_LABEL_DESCRIPTOR = msg({
	message: 'Client URL',
	comment: 'Label for Bluesky OAuth client URL input.',
});
const BLUESKY_LOGO_URI_LABEL_DESCRIPTOR = msg({
	message: 'Logo URL',
	comment: 'Label for Bluesky OAuth logo URL input.',
});
const BLUESKY_TOS_URI_LABEL_DESCRIPTOR = msg({
	message: 'Terms URL',
	comment: 'Label for Bluesky OAuth terms URL input.',
});
const BLUESKY_POLICY_URI_LABEL_DESCRIPTOR = msg({
	message: 'Privacy URL',
	comment: 'Label for Bluesky OAuth privacy URL input.',
});
const BLUESKY_KEY_ID_LABEL_DESCRIPTOR = msg({
	message: 'Signing key ID',
	comment: 'Label for Bluesky OAuth signing key ID input.',
});
const BLUESKY_PRIVATE_KEY_LABEL_DESCRIPTOR = msg({
	message: 'Private key',
	comment: 'Label for Bluesky OAuth private key input.',
});

function buildIntegrationModeOptions(
	i18n: ReturnType<typeof useLingui>['i18n'],
): ReadonlyArray<RadioOption<IntegrationSetupMode>> {
	return [
		{
			value: 'later',
			name: i18n._(INTEGRATION_LATER_NAME_DESCRIPTOR),
			desc: i18n._(INTEGRATION_LATER_DESC_DESCRIPTOR),
		},
		{
			value: 'configure',
			name: i18n._(INTEGRATION_CONFIGURE_NAME_DESCRIPTOR),
			desc: i18n._(INTEGRATION_CONFIGURE_DESC_DESCRIPTOR),
		},
	];
}

function getIntegrationMode(draft: ServiceIntegrationDraft, kind: IntegrationStepKind): IntegrationSetupMode {
	switch (kind) {
		case 'gif':
			return draft.gifMode;
		case 'youtube':
			return draft.youtubeMode;
		case 'captcha':
			return draft.captchaMode;
		case 'email':
			return draft.emailMode;
		case 'bluesky':
			return draft.blueskyMode;
	}
}

function getIntegrationCopy(kind: IntegrationStepKind): {title: MessageDescriptor; body: MessageDescriptor} {
	switch (kind) {
		case 'gif':
			return {title: GIF_SETUP_TITLE_DESCRIPTOR, body: GIF_SETUP_BODY_DESCRIPTOR};
		case 'youtube':
			return {title: YOUTUBE_SETUP_TITLE_DESCRIPTOR, body: YOUTUBE_SETUP_BODY_DESCRIPTOR};
		case 'captcha':
			return {title: CAPTCHA_SETUP_TITLE_DESCRIPTOR, body: CAPTCHA_SETUP_BODY_DESCRIPTOR};
		case 'email':
			return {title: EMAIL_SETUP_TITLE_DESCRIPTOR, body: EMAIL_SETUP_BODY_DESCRIPTOR};
		case 'bluesky':
			return {title: BLUESKY_SETUP_TITLE_DESCRIPTOR, body: BLUESKY_SETUP_BODY_DESCRIPTOR};
	}
}

function parseSmtpPort(value: string): number | null {
	const parsed = Number.parseInt(value, 10);
	return Number.isInteger(parsed) && parsed >= 1 && parsed <= 65535 ? parsed : null;
}

const MEDIA_EXPIRY_REVEAL_TRANSITION: Transition = {
	type: 'spring',
	stiffness: 460,
	damping: 40,
	mass: 0.7,
};

export const MediaExpiryStep = observer(
	({
		draft,
		disabled,
		onDraftChange,
	}: {
		draft: MediaExpiryDraft;
		disabled: boolean;
		onDraftChange: (patch: Partial<MediaExpiryDraft>) => void;
	}) => {
		const {i18n} = useLingui();
		const prefersReducedMotion = useReducedMotion();
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.media-expiry-step">
				<StepHeader title={i18n._(MEDIA_EXPIRY_TITLE_DESCRIPTOR)} body={i18n._(MEDIA_EXPIRY_BODY_DESCRIPTOR)} />
				<Switch
					label={i18n._(MEDIA_EXPIRY_ENABLE_LABEL_DESCRIPTOR)}
					description={i18n._(MEDIA_EXPIRY_ENABLE_DESC_DESCRIPTOR)}
					value={draft.enabled}
					onChange={(value) => onDraftChange({enabled: value})}
					disabled={disabled}
					data-flx="app.self-hosted-setup-wizard-gate.media-expiry-switch"
				/>
				<AnimatePresence initial={false}>
					{draft.enabled && (
						<motion.div
							key="media-expiry-fields"
							className={styles.collapsibleReveal}
							initial={{height: 0, opacity: 0}}
							animate={{height: 'auto', opacity: 1}}
							exit={{height: 0, opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : MEDIA_EXPIRY_REVEAL_TRANSITION}
							data-flx="app.self-hosted-setup-wizard-gate.media-expiry-reveal"
						>
							<div
								className={styles.integrationFields}
								data-flx="app.self-hosted-setup-wizard-gate.media-expiry-fields"
							>
								<div className={styles.integrationGrid}>
									<Input
										label={i18n._(MEDIA_MIN_SIZE_LABEL_DESCRIPTOR)}
										inputMode="decimal"
										value={draft.minSizeMb}
										onChange={(event) => onDraftChange({minSizeMb: event.target.value})}
										disabled={disabled}
									/>
									<Input
										label={i18n._(MEDIA_MAX_SIZE_LABEL_DESCRIPTOR)}
										inputMode="decimal"
										value={draft.maxSizeMb}
										onChange={(event) => onDraftChange({maxSizeMb: event.target.value})}
										disabled={disabled}
									/>
								</div>
								<Input
									label={i18n._(MEDIA_MAX_ELIGIBLE_SIZE_LABEL_DESCRIPTOR)}
									inputMode="decimal"
									value={draft.maxEligibleSizeMb}
									onChange={(event) => onDraftChange({maxEligibleSizeMb: event.target.value})}
									disabled={disabled}
								/>
								<div className={styles.integrationGrid}>
									<Input
										label={i18n._(MEDIA_MIN_LIFETIME_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.minLifetimeDays}
										onChange={(event) => onDraftChange({minLifetimeDays: event.target.value})}
										disabled={disabled}
									/>
									<Input
										label={i18n._(MEDIA_MAX_LIFETIME_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.maxLifetimeDays}
										onChange={(event) => onDraftChange({maxLifetimeDays: event.target.value})}
										disabled={disabled}
									/>
								</div>
								<div className={styles.integrationGrid}>
									<Input
										label={i18n._(MEDIA_CURVE_LABEL_DESCRIPTOR)}
										inputMode="decimal"
										value={draft.curve}
										onChange={(event) => onDraftChange({curve: event.target.value})}
										disabled={disabled}
									/>
									<Input
										label={i18n._(MEDIA_RENEW_THRESHOLD_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.renewThresholdDays}
										onChange={(event) => onDraftChange({renewThresholdDays: event.target.value})}
										disabled={disabled}
									/>
								</div>
								<Input
									label={i18n._(MEDIA_RENEW_WINDOW_LABEL_DESCRIPTOR)}
									inputMode="numeric"
									value={draft.renewWindowDays}
									onChange={(event) => onDraftChange({renewWindowDays: event.target.value})}
									disabled={disabled}
								/>
							</div>
						</motion.div>
					)}
				</AnimatePresence>
			</section>
		);
	},
);

export const IntegrationStep = observer(
	({
		kind,
		draft,
		disabled,
		smtpTesting,
		smtpTestResult,
		onDraftChange,
		onTestSmtp,
	}: {
		kind: IntegrationStepKind;
		draft: ServiceIntegrationDraft;
		disabled: boolean;
		smtpTesting: boolean;
		smtpTestResult: string | null;
		onDraftChange: (patch: Partial<ServiceIntegrationDraft>) => void;
		onTestSmtp: () => void;
	}) => {
		const {i18n} = useLingui();
		const copy = getIntegrationCopy(kind);
		const mode = getIntegrationMode(draft, kind);
		const setMode = useCallback(
			(next: IntegrationSetupMode) => {
				switch (kind) {
					case 'gif':
						onDraftChange({gifMode: next});
						break;
					case 'youtube':
						onDraftChange({youtubeMode: next});
						break;
					case 'captcha':
						onDraftChange({captchaMode: next});
						break;
					case 'email':
						onDraftChange({emailMode: next});
						break;
					case 'bluesky':
						onDraftChange({blueskyMode: next});
						break;
				}
			},
			[kind, onDraftChange],
		);
		const captchaOptions: ReadonlyArray<RadioOption<CaptchaProvider>> = [
			{value: 'hcaptcha', name: i18n._(HCAPTCHA_NAME_DESCRIPTOR), desc: i18n._(CAPTCHA_SETUP_BODY_DESCRIPTOR)},
			{value: 'turnstile', name: i18n._(TURNSTILE_NAME_DESCRIPTOR), desc: i18n._(CAPTCHA_SETUP_BODY_DESCRIPTOR)},
		];
		return (
			<section className={styles.step} data-flx={`app.self-hosted-setup-wizard-gate.integration-${kind}-step`}>
				<StepHeader title={i18n._(copy.title)} body={i18n._(copy.body)} />
				<RadioGroup
					options={buildIntegrationModeOptions(i18n)}
					value={mode}
					onChange={setMode}
					disabled={disabled}
					aria-label={i18n._(copy.title)}
				/>
				{mode === 'configure' && (
					<div className={styles.integrationFields} data-flx="app.self-hosted-setup-wizard-gate.integration-fields">
						{kind === 'gif' && (
							<Input
								label={i18n._(API_KEY_LABEL_DESCRIPTOR)}
								value={draft.klipyApiKey}
								onChange={(event) => onDraftChange({klipyApiKey: event.target.value})}
								disabled={disabled}
							/>
						)}
						{kind === 'youtube' && (
							<Input
								label={i18n._(API_KEY_LABEL_DESCRIPTOR)}
								value={draft.youtubeApiKey}
								onChange={(event) => onDraftChange({youtubeApiKey: event.target.value})}
								disabled={disabled}
							/>
						)}
						{kind === 'captcha' && (
							<>
								<div className={styles.fieldLabel}>{i18n._(PROVIDER_LABEL_DESCRIPTOR)}</div>
								<RadioGroup
									options={captchaOptions}
									value={draft.captchaProvider}
									onChange={(value) => onDraftChange({captchaProvider: value})}
									disabled={disabled}
									aria-label={i18n._(PROVIDER_LABEL_DESCRIPTOR)}
								/>
								<Input
									label={i18n._(SITE_KEY_LABEL_DESCRIPTOR)}
									value={draft.captchaProvider === 'hcaptcha' ? draft.hcaptchaSiteKey : draft.turnstileSiteKey}
									onChange={(event) =>
										onDraftChange(
											draft.captchaProvider === 'hcaptcha'
												? {hcaptchaSiteKey: event.target.value}
												: {turnstileSiteKey: event.target.value},
										)
									}
									disabled={disabled}
								/>
								<Input
									label={i18n._(SECRET_KEY_LABEL_DESCRIPTOR)}
									type="password"
									value={draft.captchaProvider === 'hcaptcha' ? draft.hcaptchaSecretKey : draft.turnstileSecretKey}
									onChange={(event) =>
										onDraftChange(
											draft.captchaProvider === 'hcaptcha'
												? {hcaptchaSecretKey: event.target.value}
												: {turnstileSecretKey: event.target.value},
										)
									}
									disabled={disabled}
								/>
							</>
						)}
						{kind === 'email' && (
							<>
								<Switch
									label={i18n._(ENABLE_EMAIL_LABEL_DESCRIPTOR)}
									value={draft.emailEnabled}
									onChange={(value) => onDraftChange({emailEnabled: value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(EMAIL_FROM_EMAIL_LABEL_DESCRIPTOR)}
									value={draft.emailFromEmail}
									onChange={(event) => onDraftChange({emailFromEmail: event.target.value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(EMAIL_FROM_NAME_LABEL_DESCRIPTOR)}
									value={draft.emailFromName}
									onChange={(event) => onDraftChange({emailFromName: event.target.value})}
									disabled={disabled}
								/>
								<div className={styles.integrationGrid}>
									<Input
										label={i18n._(SMTP_HOST_LABEL_DESCRIPTOR)}
										value={draft.smtpHost}
										onChange={(event) => onDraftChange({smtpHost: event.target.value})}
										disabled={disabled}
									/>
									<Input
										label={i18n._(SMTP_PORT_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.smtpPort}
										onChange={(event) => onDraftChange({smtpPort: event.target.value})}
										disabled={disabled}
									/>
								</div>
								<Input
									label={i18n._(SMTP_USERNAME_LABEL_DESCRIPTOR)}
									value={draft.smtpUsername}
									onChange={(event) => onDraftChange({smtpUsername: event.target.value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(SMTP_PASSWORD_LABEL_DESCRIPTOR)}
									type="password"
									value={draft.smtpPassword}
									onChange={(event) => onDraftChange({smtpPassword: event.target.value})}
									disabled={disabled}
								/>
								<Switch
									label={i18n._(SMTP_SECURE_LABEL_DESCRIPTOR)}
									value={draft.smtpSecure}
									onChange={(value) => onDraftChange({smtpSecure: value})}
									disabled={disabled}
								/>
								<div className={styles.integrationActionRow}>
									<Button
										variant="secondary"
										small
										submitting={smtpTesting}
										disabled={
											disabled ||
											!draft.smtpHost.trim() ||
											!parseSmtpPort(draft.smtpPort) ||
											!draft.smtpUsername.trim() ||
											!draft.smtpPassword.trim()
										}
										onClick={onTestSmtp}
									>
										{i18n._(SMTP_TEST_DESCRIPTOR)}
									</Button>
									{smtpTestResult && (
										<span className={styles.integrationStatus} role="status">
											{smtpTestResult === 'ok' ? i18n._(SMTP_TEST_OK_DESCRIPTOR) : smtpTestResult}
										</span>
									)}
								</div>
							</>
						)}
						{kind === 'bluesky' && (
							<>
								<Switch
									label={i18n._(BLUESKY_ENABLED_LABEL_DESCRIPTOR)}
									value={draft.blueskyEnabled}
									onChange={(value) => onDraftChange({blueskyEnabled: value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(BLUESKY_CLIENT_NAME_LABEL_DESCRIPTOR)}
									value={draft.blueskyClientName}
									onChange={(event) => onDraftChange({blueskyClientName: event.target.value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(BLUESKY_CLIENT_URI_LABEL_DESCRIPTOR)}
									value={draft.blueskyClientUri}
									onChange={(event) => onDraftChange({blueskyClientUri: event.target.value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(BLUESKY_LOGO_URI_LABEL_DESCRIPTOR)}
									value={draft.blueskyLogoUri}
									onChange={(event) => onDraftChange({blueskyLogoUri: event.target.value})}
									disabled={disabled}
								/>
								<div className={styles.integrationGrid}>
									<Input
										label={i18n._(BLUESKY_TOS_URI_LABEL_DESCRIPTOR)}
										value={draft.blueskyTosUri}
										onChange={(event) => onDraftChange({blueskyTosUri: event.target.value})}
										disabled={disabled}
									/>
									<Input
										label={i18n._(BLUESKY_POLICY_URI_LABEL_DESCRIPTOR)}
										value={draft.blueskyPolicyUri}
										onChange={(event) => onDraftChange({blueskyPolicyUri: event.target.value})}
										disabled={disabled}
									/>
								</div>
								<Input
									label={i18n._(BLUESKY_KEY_ID_LABEL_DESCRIPTOR)}
									value={draft.blueskyKeyId}
									onChange={(event) => onDraftChange({blueskyKeyId: event.target.value})}
									disabled={disabled}
								/>
								<Input
									label={i18n._(BLUESKY_PRIVATE_KEY_LABEL_DESCRIPTOR)}
									type="password"
									value={draft.blueskyPrivateKey}
									onChange={(event) => onDraftChange({blueskyPrivateKey: event.target.value})}
									disabled={disabled}
								/>
							</>
						)}
					</div>
				)}
			</section>
		);
	},
);

export const ServicesStep = observer(
	({
		available,
		selection,
		disabled,
		onToggle,
	}: {
		available: ServiceAvailability;
		selection: ServiceSelection;
		disabled: boolean;
		onToggle: (service: keyof ServiceSelection, value: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		const anyAvailable = available.gif || available.youtube || available.bluesky;
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.services-step">
				<StepHeader title={i18n._(SERVICES_TITLE_DESCRIPTOR)} body={i18n._(SERVICES_BODY_DESCRIPTOR)} />
				{!anyAvailable && (
					<p className={styles.emptyNote} data-flx="app.self-hosted-setup-wizard-gate.services-empty">
						{i18n._(SERVICES_NONE_DESCRIPTOR)}
					</p>
				)}
				{available.gif && (
					<Switch
						label={i18n._(SERVICE_GIF_LABEL_DESCRIPTOR)}
						description={i18n._(SERVICE_GIF_DESC_DESCRIPTOR)}
						value={selection.gif}
						onChange={(value) => onToggle('gif', value)}
						disabled={disabled}
						data-flx="app.self-hosted-setup-wizard-gate.service-gif-switch"
					/>
				)}
				{available.youtube && (
					<Switch
						label={i18n._(SERVICE_YOUTUBE_LABEL_DESCRIPTOR)}
						description={i18n._(SERVICE_YOUTUBE_DESC_DESCRIPTOR)}
						value={selection.youtube}
						onChange={(value) => onToggle('youtube', value)}
						disabled={disabled}
						data-flx="app.self-hosted-setup-wizard-gate.service-youtube-switch"
					/>
				)}
				{available.bluesky && (
					<Switch
						label={i18n._(SERVICE_BLUESKY_LABEL_DESCRIPTOR)}
						description={i18n._(SERVICE_BLUESKY_DESC_DESCRIPTOR)}
						value={selection.bluesky}
						onChange={(value) => onToggle('bluesky', value)}
						disabled={disabled}
						data-flx="app.self-hosted-setup-wizard-gate.service-bluesky-switch"
					/>
				)}
			</section>
		);
	},
);

export const PremiumStep = observer(
	({mode, disabled, onChange}: {mode: PremiumMode; disabled: boolean; onChange: (mode: PremiumMode) => void}) => {
		const {i18n} = useLingui();
		const options: ReadonlyArray<RadioOption<PremiumMode>> = [
			{
				value: 'mirror',
				name: i18n._(PREMIUM_MIRROR_NAME_DESCRIPTOR),
				desc: i18n._(PREMIUM_MIRROR_DESC_DESCRIPTOR),
			},
			{
				value: 'everyone',
				name: i18n._(PREMIUM_EVERYONE_NAME_DESCRIPTOR),
				desc: i18n._(PREMIUM_EVERYONE_DESC_DESCRIPTOR),
			},
		];
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.premium-step">
				<StepHeader title={i18n._(PREMIUM_TITLE_DESCRIPTOR)} body={i18n._(PREMIUM_BODY_DESCRIPTOR)} />
				<RadioGroup
					options={options}
					value={mode}
					onChange={onChange}
					disabled={disabled}
					aria-label={i18n._(PREMIUM_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.premium-radio-group"
				/>
			</section>
		);
	},
);

interface SummaryRowProps {
	label: string;
	value: string;
}

const SummaryRow: React.FC<SummaryRowProps> = ({label, value}) => (
	<div className={styles.summaryRow} data-flx="app.self-hosted-setup-wizard-gate.summary-row">
		<span className={styles.summaryLabel} data-flx="app.self-hosted-setup-wizard-gate.summary-label">
			{label}
		</span>
		<span className={styles.summaryValue} data-flx="app.self-hosted-setup-wizard-gate.summary-value">
			{value}
		</span>
	</div>
);

export const FinishStep = observer(
	({
		productName,
		registrationMode,
		singleCommunityEnabled,
		directMessagesDisabled,
		attachmentExpiryEnabled,
		premiumMode,
		submitError,
	}: {
		productName: string;
		registrationMode: RegistrationMode;
		singleCommunityEnabled: boolean;
		directMessagesDisabled: boolean;
		attachmentExpiryEnabled: boolean;
		premiumMode: PremiumMode;
		submitError: string | null;
	}) => {
		const {i18n} = useLingui();
		const registrationLabel =
			registrationMode === 'open'
				? i18n._(REGISTRATION_OPEN_NAME_DESCRIPTOR)
				: registrationMode === 'approval'
					? i18n._(REGISTRATION_APPROVAL_NAME_DESCRIPTOR)
					: i18n._(REGISTRATION_CLOSED_NAME_DESCRIPTOR);
		const premiumLabel =
			premiumMode === 'mirror' ? i18n._(PREMIUM_MIRROR_NAME_DESCRIPTOR) : i18n._(PREMIUM_EVERYONE_NAME_DESCRIPTOR);
		const onLabel = i18n._(SUMMARY_ON_DESCRIPTOR);
		const offLabel = i18n._(SUMMARY_OFF_DESCRIPTOR);
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.finish-step">
				<StepHeader title={i18n._(FINISH_TITLE_DESCRIPTOR)} body={i18n._(FINISH_BODY_DESCRIPTOR)} />
				<div className={styles.summary} data-flx="app.self-hosted-setup-wizard-gate.summary">
					<SummaryRow label={i18n._(SUMMARY_PRODUCT_NAME_DESCRIPTOR)} value={productName} />
					<SummaryRow label={i18n._(SUMMARY_REGISTRATION_DESCRIPTOR)} value={registrationLabel} />
					<SummaryRow
						label={i18n._(SUMMARY_SINGLE_COMMUNITY_DESCRIPTOR)}
						value={singleCommunityEnabled ? onLabel : offLabel}
					/>
					<SummaryRow
						label={i18n._(SUMMARY_DIRECT_MESSAGES_DESCRIPTOR)}
						value={directMessagesDisabled ? offLabel : onLabel}
					/>
					<SummaryRow
						label={i18n._(SUMMARY_ATTACHMENT_EXPIRY_DESCRIPTOR)}
						value={attachmentExpiryEnabled ? onLabel : offLabel}
					/>
					<SummaryRow label={i18n._(SUMMARY_PREMIUM_DESCRIPTOR)} value={premiumLabel} />
				</div>
				{submitError && (
					<p className={styles.submitError} role="alert" data-flx="app.self-hosted-setup-wizard-gate.submit-error">
						{submitError}
					</p>
				)}
			</section>
		);
	},
);
