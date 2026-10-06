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
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {ThemeSelector} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeTabContent';
import {LanguageSelector} from '@app/features/user/components/modals/tabs/LanguageTab';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {
	type AccountIdentityMode,
	AccountIdentityModes,
	type TagStyle,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ImageIcon, TrashIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion, type Transition, useReducedMotion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const PUSH_RELAY_NOTICE_URL = 'https://fluxer.com/push-relay';

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
const SIGN_IN_METHOD_TITLE_DESCRIPTOR = msg({
	message: 'Choose how people sign in',
	comment: 'Setup wizard title for choosing whether accounts use usernames or email addresses to sign in.',
});
const SIGN_IN_METHOD_BODY_DESCRIPTOR = msg({
	message: 'You cannot change this once the first account exists.',
	comment: 'Setup wizard body for the sign-in method step.',
});
const SIGN_IN_METHOD_USERNAME_NAME_DESCRIPTOR = msg({
	message: 'Usernames (recommended)',
	comment: 'Setup wizard option where people sign in with a username and password and no email is collected.',
});
const SIGN_IN_METHOD_USERNAME_DESC_DESCRIPTOR = msg({
	message:
		'People sign in with a username and password. No email address is collected. Each person can save a recovery kit in case they forget their password.',
	comment: 'Description for the username sign-in option in the setup wizard.',
});
const SIGN_IN_METHOD_USERNAME_MOBILE_NOTICE_DESCRIPTOR = msg({
	message: 'Username sign-in is still rolling out to the mobile apps. Full support on every platform is coming soon.',
	comment: 'Warning shown in the setup wizard when the username sign-in option is selected. Keep it short and plain.',
});
const SIGN_IN_METHOD_EMAIL_NAME_DESCRIPTOR = msg({
	message: 'Email',
	comment: 'Setup wizard option where people sign in with an email address and password.',
});
const SIGN_IN_METHOD_EMAIL_DESC_DESCRIPTOR = msg({
	message:
		'People sign in with an email address and password. Password resets and verification need email delivery, which you can set up later.',
	comment: 'Description for the email sign-in option in the setup wizard.',
});
const SIGN_IN_METHOD_USERNAME_SUMMARY_DESCRIPTOR = msg({
	message: 'Usernames',
	comment: 'Summary value on the setup wizard finish step when people sign in with a username.',
});
const TAG_STYLE_NONE_SUMMARY_DESCRIPTOR = msg({
	message: 'No tags',
	comment: 'Setup wizard finish summary value when usernames are unique and shown without a tag.',
});
const TAG_STYLE_RANDOM_SUMMARY_DESCRIPTOR = msg({
	message: 'Random tags',
	comment: 'Setup wizard finish summary value when every username gets a random four digit tag.',
});
const TAG_STYLE_TITLE_DESCRIPTOR = msg({
	message: 'Username tags',
	comment: 'Heading above the choice of how usernames are tagged in the setup wizard.',
});
const TAG_STYLE_NONE_NAME_DESCRIPTOR = msg({
	message: 'No tags (recommended)',
	comment: 'Setup wizard option where every username is unique and shown without a tag.',
});
const TAG_STYLE_NONE_EMAIL_DESC_DESCRIPTOR = msg({
	message: 'Each name belongs to one person and shows on its own, like alex.',
	comment: 'Description for the no tags option when people sign in with email. Keep the example name alex.',
});
const TAG_STYLE_RANDOM_NAME_DESCRIPTOR = msg({
	message: 'Random tags',
	comment: 'Setup wizard option where every username gets a random four digit tag.',
});
const TAG_STYLE_RANDOM_EMAIL_DESC_DESCRIPTOR = msg({
	message: 'Names get a random tag, like alex#4821, so several people can share a name.',
	comment: 'Description for the random tags option when people sign in with email. Keep alex#4821.',
});
const ADMIN_RECOVERY_KIT_TITLE_DESCRIPTOR = msg({
	message: 'Save your recovery kit',
	comment: 'Setup wizard title asking the administrator to create an account recovery kit.',
});
const ADMIN_RECOVERY_KIT_BODY_DESCRIPTOR = msg({
	message:
		'Your account has no email address. If you forget your password, a recovery kit is the only way back in. Create one now and keep it somewhere safe.',
	comment: 'Setup wizard body explaining why the administrator needs a recovery kit.',
});
const ADMIN_RECOVERY_KIT_CREATE_DESCRIPTOR = msg({
	message: 'Create recovery kit',
	comment: 'Button in the setup wizard that creates a recovery kit for the administrator account.',
});
const ADMIN_RECOVERY_KIT_RECREATE_DESCRIPTOR = msg({
	message: 'Create a new kit',
	comment: 'Button in the setup wizard that replaces the administrator recovery kit with a new one.',
});
const ADMIN_RECOVERY_KIT_SAVED_DESCRIPTOR = msg({
	message: 'Your recovery kit is ready. You can continue.',
	comment: 'Status shown in the setup wizard once the administrator has a recovery kit.',
});
const ADMIN_RECOVERY_KIT_MISSING_DESCRIPTOR = msg({
	message: 'Create a recovery kit to continue.',
	comment: 'Status shown in the setup wizard while the administrator has no recovery kit.',
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
	context: 'registration-policy',
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
	message: 'Attachment expiration',
	comment: 'Setup wizard media expiry step title.',
});
const MEDIA_EXPIRY_BODY_DESCRIPTOR = msg({
	message: 'Choose whether uploaded attachments should expire automatically based on size.',
	comment: 'Setup wizard media expiry step body.',
});
const MEDIA_EXPIRY_ENABLE_LABEL_DESCRIPTOR = msg({
	message: 'Enable attachment expiration',
	comment: 'Label for enabling attachment expiry during setup.',
});
const MEDIA_EXPIRY_ENABLE_DESC_DESCRIPTOR = msg({
	message: 'When attachment expiration is disabled, attachments remain available until manually removed.',
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
	message: 'Expiration curve',
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

const PUSH_RELAY_TITLE_DESCRIPTOR = msg({
	message: 'Mobile push notifications',
	comment: 'Setup wizard push relay consent step title.',
});
const PUSH_RELAY_BODY_DESCRIPTOR = msg({
	message:
		"The official Fluxer mobile apps receive notifications through Fluxer's push relay, which hands them to Apple and Google. Self-hosted UnifiedPush and ntfy endpoints never reach the relay and need no agreement.",
	comment: 'Setup wizard push relay consent step body.',
});
const PUSH_RELAY_ACCEPT_LABEL_DESCRIPTOR = msg({
	message: 'Accept the push relay supplemental privacy notice',
	comment: 'Label for the push relay consent switch during setup.',
});
const PUSH_RELAY_ACCEPT_DESC_DESCRIPTOR = msg({
	message:
		'Leaving this off keeps the relay unused and drops notifications to the official mobile apps. You can accept it later in the admin panel.',
	comment: 'Description for the push relay consent switch during setup.',
});
const PUSH_RELAY_NOTICE_LINK_DESCRIPTOR = msg({
	message: 'Read the supplemental privacy notice',
	comment: 'Link to the push relay supplemental privacy notice shown during setup.',
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
	message:
		'Keep free and premium tiers. You can customize the tiers, sell premium through Stripe, or hand out gift codes later from the admin panel.',
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
	message: 'Attachment expiration',
	comment: 'Summary row label for the attachment expiry choice in the setup wizard.',
});
const SUMMARY_PUSH_RELAY_DESCRIPTOR = msg({
	message: 'Push relay notice',
	comment: 'Summary row label for the push relay consent on the setup wizard finish step.',
});
const SUMMARY_PREMIUM_DESCRIPTOR = msg({
	message: 'Premium model',
	comment: 'Summary row label for the premium model in the setup wizard.',
});
const SUMMARY_SIGN_IN_METHOD_DESCRIPTOR = msg({
	message: 'Sign-in method',
	comment: 'Summary row label for how people sign in, on the setup wizard finish step.',
});
const SUMMARY_ACCEPTED_DESCRIPTOR = msg({
	message: 'Accepted',
	comment: 'Summary value when the operator accepted the push relay notice.',
});
const SUMMARY_NOT_ACCEPTED_DESCRIPTOR = msg({
	message: 'Not accepted',
	comment: 'Summary value when the operator left the push relay notice unaccepted.',
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
						<AnimatePresence
							mode="wait"
							initial={false}
							data-flx="app.setup.setup-wizard-steps.welcome-step.animate-presence"
						>
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
						data-flx="app.setup.setup-wizard-steps.welcome-step.locale-selector.set-local-locale"
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
				<StepHeader
					title={themeLabel}
					body={i18n._(THEME_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.theme-step.step-header"
				/>
				<div className={styles.themeBlock} data-flx="app.self-hosted-setup-wizard-gate.theme-block">
					<ThemeSelector
						value={theme}
						onChange={onThemeChange}
						ariaLabel={themeLabel}
						className={styles.themeButtonGroup}
						data-flx="app.setup.setup-wizard-steps.theme-step.theme-button-group.theme-change"
					/>
				</div>
			</section>
		);
	},
);

export const SignInMethodStep = observer(
	({
		mode,
		tagStyle,
		disabled,
		error,
		onChange,
		onTagStyleChange,
	}: {
		mode: AccountIdentityMode;
		tagStyle: TagStyle;
		disabled: boolean;
		error: string | null;
		onChange: (mode: AccountIdentityMode) => void;
		onTagStyleChange: (tagStyle: TagStyle) => void;
	}) => {
		const {i18n} = useLingui();
		const options: ReadonlyArray<RadioOption<AccountIdentityMode>> = [
			{
				value: AccountIdentityModes.USERNAME,
				name: i18n._(SIGN_IN_METHOD_USERNAME_NAME_DESCRIPTOR),
				desc: i18n._(SIGN_IN_METHOD_USERNAME_DESC_DESCRIPTOR),
			},
			{
				value: AccountIdentityModes.EMAIL,
				name: i18n._(SIGN_IN_METHOD_EMAIL_NAME_DESCRIPTOR),
				desc: i18n._(SIGN_IN_METHOD_EMAIL_DESC_DESCRIPTOR),
			},
		];
		const usernameSignIn = mode === AccountIdentityModes.USERNAME;
		const tagStyleOptions: ReadonlyArray<RadioOption<TagStyle>> = [
			{
				value: TagStyles.NONE,
				name: i18n._(TAG_STYLE_NONE_NAME_DESCRIPTOR),
				desc: i18n._(TAG_STYLE_NONE_EMAIL_DESC_DESCRIPTOR),
			},
			{
				value: TagStyles.RANDOM,
				name: i18n._(TAG_STYLE_RANDOM_NAME_DESCRIPTOR),
				desc: i18n._(TAG_STYLE_RANDOM_EMAIL_DESC_DESCRIPTOR),
			},
		];
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.sign-in-method-step">
				<StepHeader
					title={i18n._(SIGN_IN_METHOD_TITLE_DESCRIPTOR)}
					body={i18n._(SIGN_IN_METHOD_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.sign-in-method-step.step-header"
				/>
				<RadioGroup
					options={options}
					value={mode}
					onChange={onChange}
					disabled={disabled}
					aria-label={i18n._(SIGN_IN_METHOD_TITLE_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.sign-in-method-radio-group"
				/>
				{usernameSignIn && (
					<WarningAlert data-flx="app.self-hosted-setup-wizard-gate.username-mobile-notice">
						{i18n._(SIGN_IN_METHOD_USERNAME_MOBILE_NOTICE_DESCRIPTOR)}
					</WarningAlert>
				)}
				{!usernameSignIn && (
					<div className={styles.usernameStyle} data-flx="app.self-hosted-setup-wizard-gate.username-style">
						<h3 className={styles.usernameStyleTitle}>{i18n._(TAG_STYLE_TITLE_DESCRIPTOR)}</h3>
						<RadioGroup
							options={tagStyleOptions}
							value={tagStyle}
							onChange={onTagStyleChange}
							disabled={disabled}
							aria-label={i18n._(TAG_STYLE_TITLE_DESCRIPTOR)}
							data-flx="app.self-hosted-setup-wizard-gate.username-style-radio-group"
						/>
					</div>
				)}
				{error && (
					<p
						className={styles.submitError}
						role="alert"
						data-flx="app.self-hosted-setup-wizard-gate.sign-in-method-error"
					>
						{error}
					</p>
				)}
			</section>
		);
	},
);

export const AdminIntroStep = observer(() => {
	const {i18n} = useLingui();
	return (
		<section className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.admin-intro-step">
			<StepHeader
				title={i18n._(WELCOME_UNAUTHED_TITLE_DESCRIPTOR)}
				body={i18n._(WELCOME_UNAUTHED_BODY_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.admin-intro-step.step-header"
			/>
		</section>
	);
});

export const AdminAccountStep = observer(({theme, usernameSignIn}: {theme: ThemeType; usernameSignIn: boolean}) => {
	const {i18n} = useLingui();
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.admin-account-step">
			<div className={styles.accountSetupForm} data-flx="app.self-hosted-setup-wizard-gate.admin-form">
				<AuthRegisterFormCore
					fields={{
						showEmail: !usernameSignIn,
						showPassword: true,
						showPasswordConfirmation: true,
						showUsernameValidation: true,
						requireUsername: usernameSignIn,
					}}
					submitLabel={i18n._(CREATE_ADMIN_ACCOUNT_DESCRIPTOR)}
					redirectPath=""
					offerRecoveryKit={usernameSignIn}
					theme={theme}
					showLegalConsent={false}
					data-flx="app.self-hosted-setup-wizard-gate.admin-register-form"
				/>
			</div>
		</section>
	);
});

export const AdminRecoveryKitStep = observer(
	({hasRecoveryKit, creating, onCreate}: {hasRecoveryKit: boolean; creating: boolean; onCreate: () => void}) => {
		const {i18n} = useLingui();
		return (
			<section className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.admin-recovery-kit-step">
				<StepHeader
					title={i18n._(ADMIN_RECOVERY_KIT_TITLE_DESCRIPTOR)}
					body={i18n._(ADMIN_RECOVERY_KIT_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.admin-recovery-kit-step.step-header"
				/>
				<Button
					variant={hasRecoveryKit ? 'secondary' : 'primary'}
					submitting={creating}
					onClick={onCreate}
					data-flx="app.self-hosted-setup-wizard-gate.admin-recovery-kit-step.button.create"
				>
					{hasRecoveryKit
						? i18n._(ADMIN_RECOVERY_KIT_RECREATE_DESCRIPTOR)
						: i18n._(ADMIN_RECOVERY_KIT_CREATE_DESCRIPTOR)}
				</Button>
				<p className={styles.body} role="status" data-flx="app.self-hosted-setup-wizard-gate.admin-recovery-kit-status">
					{hasRecoveryKit ? i18n._(ADMIN_RECOVERY_KIT_SAVED_DESCRIPTOR) : i18n._(ADMIN_RECOVERY_KIT_MISSING_DESCRIPTOR)}
				</p>
			</section>
		);
	},
);

export const LoadingStep = observer(() => {
	const {i18n} = useLingui();
	return (
		<section className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.loading-step">
			<Spinner size="large" data-flx="app.self-hosted-setup-wizard-gate.loading-spinner" />
			<StepHeader
				title={i18n._(LOADING_TITLE_DESCRIPTOR)}
				body={i18n._(LOADING_BODY_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.loading-step.step-header"
			/>
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
					leftIcon={
						<UploadSimpleIcon
							size={16}
							weight="bold"
							data-flx="app.setup.setup-wizard-steps.branding-asset-row.upload-simple-icon"
						/>
					}
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
						icon={
							<TrashIcon
								size={16}
								weight="bold"
								data-flx="app.setup.setup-wizard-steps.branding-asset-row.trash-icon"
							/>
						}
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
				<StepHeader
					title={i18n._(BRANDING_TITLE_DESCRIPTOR)}
					body={i18n._(BRANDING_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.branding-step.step-header"
				/>
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
							data-flx="app.setup.setup-wizard-steps.branding-step.branding-asset-row"
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
				<StepHeader
					title={i18n._(REGISTRATION_TITLE_DESCRIPTOR)}
					body={i18n._(REGISTRATION_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.registration-step.step-header"
				/>
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
				<StepHeader
					title={i18n._(COMMUNITY_TITLE_DESCRIPTOR)}
					body={i18n._(COMMUNITY_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.community-step.step-header"
				/>
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

export type IntegrationStepKind = 'gif' | 'youtube' | 'email' | 'bluesky';
export type IntegrationSetupMode = 'later' | 'configure';

export interface ServiceIntegrationDraft {
	gifMode: IntegrationSetupMode;
	klipyApiKey: string;
	youtubeMode: IntegrationSetupMode;
	youtubeApiKey: string;
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
const API_KEY_LABEL_DESCRIPTOR = msg({message: 'API key', comment: 'Label for an integration API key input.'});
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
const SMTP_TEST_FAILED_DESCRIPTOR = msg({
	message: 'SMTP validation failed.',
	comment: 'Failure message after validating SMTP credentials when the server returns no error detail.',
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
				<StepHeader
					title={i18n._(MEDIA_EXPIRY_TITLE_DESCRIPTOR)}
					body={i18n._(MEDIA_EXPIRY_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.media-expiry-step.step-header"
				/>
				<Switch
					label={i18n._(MEDIA_EXPIRY_ENABLE_LABEL_DESCRIPTOR)}
					description={i18n._(MEDIA_EXPIRY_ENABLE_DESC_DESCRIPTOR)}
					value={draft.enabled}
					onChange={(value) => onDraftChange({enabled: value})}
					disabled={disabled}
					data-flx="app.self-hosted-setup-wizard-gate.media-expiry-switch"
				/>
				<AnimatePresence initial={false} data-flx="app.setup.setup-wizard-steps.media-expiry-step.animate-presence">
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
								<div
									className={styles.integrationGrid}
									data-flx="app.setup.setup-wizard-steps.media-expiry-step.integration-grid"
								>
									<Input
										label={i18n._(MEDIA_MIN_SIZE_LABEL_DESCRIPTOR)}
										inputMode="decimal"
										value={draft.minSizeMb}
										onChange={(event) => onDraftChange({minSizeMb: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change"
									/>
									<Input
										label={i18n._(MEDIA_MAX_SIZE_LABEL_DESCRIPTOR)}
										inputMode="decimal"
										value={draft.maxSizeMb}
										onChange={(event) => onDraftChange({maxSizeMb: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--2"
									/>
								</div>
								<Input
									label={i18n._(MEDIA_MAX_ELIGIBLE_SIZE_LABEL_DESCRIPTOR)}
									inputMode="decimal"
									value={draft.maxEligibleSizeMb}
									onChange={(event) => onDraftChange({maxEligibleSizeMb: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--3"
								/>
								<div
									className={styles.integrationGrid}
									data-flx="app.setup.setup-wizard-steps.media-expiry-step.integration-grid--2"
								>
									<Input
										label={i18n._(MEDIA_MIN_LIFETIME_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.minLifetimeDays}
										onChange={(event) => onDraftChange({minLifetimeDays: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--4"
									/>
									<Input
										label={i18n._(MEDIA_MAX_LIFETIME_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.maxLifetimeDays}
										onChange={(event) => onDraftChange({maxLifetimeDays: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--5"
									/>
								</div>
								<div
									className={styles.integrationGrid}
									data-flx="app.setup.setup-wizard-steps.media-expiry-step.integration-grid--3"
								>
									<Input
										label={i18n._(MEDIA_CURVE_LABEL_DESCRIPTOR)}
										inputMode="decimal"
										value={draft.curve}
										onChange={(event) => onDraftChange({curve: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--6"
									/>
									<Input
										label={i18n._(MEDIA_RENEW_THRESHOLD_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.renewThresholdDays}
										onChange={(event) => onDraftChange({renewThresholdDays: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--7"
									/>
								</div>
								<Input
									label={i18n._(MEDIA_RENEW_WINDOW_LABEL_DESCRIPTOR)}
									inputMode="numeric"
									value={draft.renewWindowDays}
									onChange={(event) => onDraftChange({renewWindowDays: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.media-expiry-step.input.draft-change--8"
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
		return (
			<section className={styles.step} data-flx={`app.self-hosted-setup-wizard-gate.integration-${kind}-step`}>
				<StepHeader
					title={i18n._(copy.title)}
					body={i18n._(copy.body)}
					data-flx="app.setup.setup-wizard-steps.integration-step.step-header"
				/>
				<RadioGroup
					options={buildIntegrationModeOptions(i18n)}
					value={mode}
					onChange={setMode}
					disabled={disabled}
					aria-label={i18n._(copy.title)}
					data-flx="app.setup.setup-wizard-steps.integration-step.radio-group.set-mode"
				/>
				{mode === 'configure' && (
					<div className={styles.integrationFields} data-flx="app.self-hosted-setup-wizard-gate.integration-fields">
						{kind === 'gif' && (
							<Input
								label={i18n._(API_KEY_LABEL_DESCRIPTOR)}
								value={draft.klipyApiKey}
								onChange={(event) => onDraftChange({klipyApiKey: event.target.value})}
								disabled={disabled}
								data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change"
							/>
						)}
						{kind === 'youtube' && (
							<Input
								label={i18n._(API_KEY_LABEL_DESCRIPTOR)}
								value={draft.youtubeApiKey}
								onChange={(event) => onDraftChange({youtubeApiKey: event.target.value})}
								disabled={disabled}
								data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--2"
							/>
						)}
						{kind === 'email' && (
							<>
								<Switch
									label={i18n._(ENABLE_EMAIL_LABEL_DESCRIPTOR)}
									value={draft.emailEnabled}
									onChange={(value) => onDraftChange({emailEnabled: value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.switch.draft-change"
								/>
								<Input
									label={i18n._(EMAIL_FROM_EMAIL_LABEL_DESCRIPTOR)}
									value={draft.emailFromEmail}
									onChange={(event) => onDraftChange({emailFromEmail: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--4"
								/>
								<Input
									label={i18n._(EMAIL_FROM_NAME_LABEL_DESCRIPTOR)}
									value={draft.emailFromName}
									onChange={(event) => onDraftChange({emailFromName: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--5"
								/>
								<div
									className={styles.integrationGrid}
									data-flx="app.setup.setup-wizard-steps.integration-step.integration-grid"
								>
									<Input
										label={i18n._(SMTP_HOST_LABEL_DESCRIPTOR)}
										value={draft.smtpHost}
										onChange={(event) => onDraftChange({smtpHost: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--6"
									/>
									<Input
										label={i18n._(SMTP_PORT_LABEL_DESCRIPTOR)}
										inputMode="numeric"
										value={draft.smtpPort}
										onChange={(event) => onDraftChange({smtpPort: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--7"
									/>
								</div>
								<Input
									label={i18n._(SMTP_USERNAME_LABEL_DESCRIPTOR)}
									value={draft.smtpUsername}
									onChange={(event) => onDraftChange({smtpUsername: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--8"
								/>
								<Input
									label={i18n._(SMTP_PASSWORD_LABEL_DESCRIPTOR)}
									type="password"
									value={draft.smtpPassword}
									onChange={(event) => onDraftChange({smtpPassword: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change.password--2"
								/>
								<Switch
									label={i18n._(SMTP_SECURE_LABEL_DESCRIPTOR)}
									value={draft.smtpSecure}
									onChange={(value) => onDraftChange({smtpSecure: value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.switch.draft-change--2"
								/>
								<div
									className={styles.integrationActionRow}
									data-flx="app.setup.setup-wizard-steps.integration-step.integration-action-row"
								>
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
										data-flx="app.setup.setup-wizard-steps.integration-step.button.test-smtp"
									>
										{i18n._(SMTP_TEST_DESCRIPTOR)}
									</Button>
									{smtpTestResult && (
										<span
											className={styles.integrationStatus}
											role="status"
											data-flx="app.setup.setup-wizard-steps.integration-step.integration-status"
										>
											{smtpTestResult === 'ok'
												? i18n._(SMTP_TEST_OK_DESCRIPTOR)
												: smtpTestResult === 'failed'
													? i18n._(SMTP_TEST_FAILED_DESCRIPTOR)
													: smtpTestResult}
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
									data-flx="app.setup.setup-wizard-steps.integration-step.switch.draft-change--3"
								/>
								<Input
									label={i18n._(BLUESKY_CLIENT_NAME_LABEL_DESCRIPTOR)}
									value={draft.blueskyClientName}
									onChange={(event) => onDraftChange({blueskyClientName: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--9"
								/>
								<Input
									label={i18n._(BLUESKY_CLIENT_URI_LABEL_DESCRIPTOR)}
									value={draft.blueskyClientUri}
									onChange={(event) => onDraftChange({blueskyClientUri: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--10"
								/>
								<Input
									label={i18n._(BLUESKY_LOGO_URI_LABEL_DESCRIPTOR)}
									value={draft.blueskyLogoUri}
									onChange={(event) => onDraftChange({blueskyLogoUri: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--11"
								/>
								<div
									className={styles.integrationGrid}
									data-flx="app.setup.setup-wizard-steps.integration-step.integration-grid--2"
								>
									<Input
										label={i18n._(BLUESKY_TOS_URI_LABEL_DESCRIPTOR)}
										value={draft.blueskyTosUri}
										onChange={(event) => onDraftChange({blueskyTosUri: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--12"
									/>
									<Input
										label={i18n._(BLUESKY_POLICY_URI_LABEL_DESCRIPTOR)}
										value={draft.blueskyPolicyUri}
										onChange={(event) => onDraftChange({blueskyPolicyUri: event.target.value})}
										disabled={disabled}
										data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--13"
									/>
								</div>
								<Input
									label={i18n._(BLUESKY_KEY_ID_LABEL_DESCRIPTOR)}
									value={draft.blueskyKeyId}
									onChange={(event) => onDraftChange({blueskyKeyId: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change--14"
								/>
								<Input
									label={i18n._(BLUESKY_PRIVATE_KEY_LABEL_DESCRIPTOR)}
									type="password"
									value={draft.blueskyPrivateKey}
									onChange={(event) => onDraftChange({blueskyPrivateKey: event.target.value})}
									disabled={disabled}
									data-flx="app.setup.setup-wizard-steps.integration-step.input.draft-change.password--3"
								/>
							</>
						)}
					</div>
				)}
			</section>
		);
	},
);

export const PushRelayConsentStep = observer(
	({accepted, disabled, onChange}: {accepted: boolean; disabled: boolean; onChange: (value: boolean) => void}) => {
		const {i18n} = useLingui();
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.push-relay-consent-step">
				<StepHeader
					title={i18n._(PUSH_RELAY_TITLE_DESCRIPTOR)}
					body={i18n._(PUSH_RELAY_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.push-relay-consent-step.step-header"
				/>
				<Switch
					label={i18n._(PUSH_RELAY_ACCEPT_LABEL_DESCRIPTOR)}
					description={i18n._(PUSH_RELAY_ACCEPT_DESC_DESCRIPTOR)}
					value={accepted}
					onChange={onChange}
					disabled={disabled}
					data-flx="app.self-hosted-setup-wizard-gate.push-relay-consent-switch"
				/>
				<FocusRing data-flx="app.setup.setup-wizard-steps.push-relay-consent-step.focus-ring">
					<a
						className={styles.noticeLink}
						href={PUSH_RELAY_NOTICE_URL}
						target="_blank"
						rel="noreferrer"
						data-flx="app.self-hosted-setup-wizard-gate.push-relay-notice-link"
					>
						{i18n._(PUSH_RELAY_NOTICE_LINK_DESCRIPTOR)}
					</a>
				</FocusRing>
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
				<StepHeader
					title={i18n._(SERVICES_TITLE_DESCRIPTOR)}
					body={i18n._(SERVICES_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.services-step.step-header"
				/>
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
				<StepHeader
					title={i18n._(PREMIUM_TITLE_DESCRIPTOR)}
					body={i18n._(PREMIUM_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.premium-step.step-header"
				/>
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
		accountIdentity,
		tagStyle,
		registrationMode,
		singleCommunityEnabled,
		directMessagesDisabled,
		attachmentExpiryEnabled,
		pushRelayConsentAccepted,
		premiumMode,
		submitError,
	}: {
		productName: string;
		accountIdentity: AccountIdentityMode;
		tagStyle: TagStyle;
		registrationMode: RegistrationMode;
		singleCommunityEnabled: boolean;
		directMessagesDisabled: boolean;
		attachmentExpiryEnabled: boolean;
		pushRelayConsentAccepted: boolean;
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
				<StepHeader
					title={i18n._(FINISH_TITLE_DESCRIPTOR)}
					body={i18n._(FINISH_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.finish-step.step-header"
				/>
				<div className={styles.summary} data-flx="app.self-hosted-setup-wizard-gate.summary">
					<SummaryRow
						label={i18n._(SUMMARY_PRODUCT_NAME_DESCRIPTOR)}
						value={productName}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row"
					/>
					<SummaryRow
						label={i18n._(SUMMARY_SIGN_IN_METHOD_DESCRIPTOR)}
						value={
							accountIdentity === AccountIdentityModes.USERNAME
								? i18n._(SIGN_IN_METHOD_USERNAME_SUMMARY_DESCRIPTOR)
								: i18n._(SIGN_IN_METHOD_EMAIL_NAME_DESCRIPTOR)
						}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row.sign-in-method"
					/>
					{accountIdentity === AccountIdentityModes.EMAIL && (
						<SummaryRow
							label={i18n._(TAG_STYLE_TITLE_DESCRIPTOR)}
							value={i18n._(
								tagStyle === TagStyles.NONE ? TAG_STYLE_NONE_SUMMARY_DESCRIPTOR : TAG_STYLE_RANDOM_SUMMARY_DESCRIPTOR,
							)}
							data-flx="app.setup.setup-wizard-steps.finish-step.summary-row.username-style"
						/>
					)}
					<SummaryRow
						label={i18n._(SUMMARY_REGISTRATION_DESCRIPTOR)}
						value={registrationLabel}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row--2"
					/>
					<SummaryRow
						label={i18n._(SUMMARY_SINGLE_COMMUNITY_DESCRIPTOR)}
						value={singleCommunityEnabled ? onLabel : offLabel}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row--3"
					/>
					<SummaryRow
						label={i18n._(SUMMARY_DIRECT_MESSAGES_DESCRIPTOR)}
						value={directMessagesDisabled ? offLabel : onLabel}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row--4"
					/>
					<SummaryRow
						label={i18n._(SUMMARY_ATTACHMENT_EXPIRY_DESCRIPTOR)}
						value={attachmentExpiryEnabled ? onLabel : offLabel}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row--5"
					/>
					<SummaryRow
						label={i18n._(SUMMARY_PUSH_RELAY_DESCRIPTOR)}
						value={
							pushRelayConsentAccepted ? i18n._(SUMMARY_ACCEPTED_DESCRIPTOR) : i18n._(SUMMARY_NOT_ACCEPTED_DESCRIPTOR)
						}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row--6"
					/>
					<SummaryRow
						label={i18n._(SUMMARY_PREMIUM_DESCRIPTOR)}
						value={premiumLabel}
						data-flx="app.setup.setup-wizard-steps.finish-step.summary-row--7"
					/>
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
