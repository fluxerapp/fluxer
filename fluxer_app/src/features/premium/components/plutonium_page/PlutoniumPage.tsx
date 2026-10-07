// SPDX-License-Identifier: AGPL-3.0-or-later

import '@app/app/fonts/radio_canada_big/radio-canada-big.css';
import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import {useCheckoutActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useCheckoutActions';
import {useCommunityActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useCommunityActions';
import {usePremiumData} from '@app/features/app/components/dialogs/components/plutonium/hooks/usePremiumData';
import {useSubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionActions';
import {useSubscriptionStatus} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionStatus';
import {PurchaseHistorySection} from '@app/features/app/components/dialogs/components/plutonium/PurchaseHistorySection';
import {SelfServeRefundSection} from '@app/features/app/components/dialogs/components/plutonium/SelfServeRefundSection';
import {SubscriptionCard} from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import GeoIP from '@app/features/app/state/GeoIP';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Guilds from '@app/features/guild/state/Guilds';
import {marketingUrl} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPage.module.css';
import {
	PlutoniumPageComparison,
	resolveUploadSizes,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageComparison';
import {PlutoniumPageIcon} from '@app/features/premium/components/plutonium_page/PlutoniumPageIcons';
import {CROWN_AVIF, CROWN_WEBP} from '@app/features/premium/components/plutonium_page/PlutoniumPageMedia';
import {
	BACK_DESCRIPTOR,
	CLOSING_BODY_DESCRIPTOR,
	CLOSING_GIFT_BODY_DESCRIPTOR,
	CLOSING_GIFT_TITLE_DESCRIPTOR,
	CLOSING_MEMBER_BODY_DESCRIPTOR,
	CLOSING_MEMBER_TITLE_DESCRIPTOR,
	CLOSING_TITLE_DESCRIPTOR,
	DONATE_INSTEAD_DESCRIPTOR,
	DONATION_HINT_DESCRIPTOR,
	GIFT_GRACE_DESCRIPTOR,
	GIFT_PLUTONIUM_DESCRIPTOR,
	GIFTED_THANKS_DESCRIPTOR,
	GRACE_DESCRIPTOR,
	LAPSED_DESCRIPTOR,
	MANAGE_HEADING_DESCRIPTOR,
	MANAGE_SUBSCRIPTION_DESCRIPTOR,
	MEMBER_THANKS_DESCRIPTOR,
	MONTHLY_PRICE_DESCRIPTOR,
	PAGE_LABEL_DESCRIPTOR,
	PRICE_OR_DESCRIPTOR,
	PURCHASE_BLOCKED_CLAIM_DESCRIPTOR,
	PURCHASE_BLOCKED_VERIFY_DESCRIPTOR,
	REDEEM_GIFT_CODE_DESCRIPTOR,
	REDEEM_ONLY_DESCRIPTOR,
	SAVE_PERCENT_DESCRIPTOR,
	SUBSCRIBE_MONTHLY_DESCRIPTOR,
	SUBSCRIBE_YEARLY_DESCRIPTOR,
	TAG_FOOTNOTE_DESCRIPTOR,
	TAG_FOOTNOTE_LINK_DESCRIPTOR,
	VISIONARY_THANKS_DESCRIPTOR,
	YEARLY_PRICE_DESCRIPTOR,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageMessages';
import {PlutoniumPageShowcase} from '@app/features/premium/components/plutonium_page/PlutoniumPageShowcase';
import PremiumState from '@app/features/premium/state/PremiumState';
import {
	areGiftPurchasesAvailable,
	arePremiumPurchasesAvailable,
	canServiceStripeSubscriptions,
	getPremiumProductFullName,
	getPremiumProductName,
	shouldShowPremiumFeatures,
} from '@app/features/premium/utils/PremiumUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, GiftIcon, TicketIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

type CheckoutPlan = 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year';

const FOOTNOTE_ID = 'plutonium-page-tag-footnote';
const MANAGE_ID = 'plutonium-page-manage';
const COMPARE_ID = 'plutonium-page-compare';
const SUBSCRIBE_ID = 'plutonium-page-subscribe';
const PLACEHOLDER_PRICE = '...';

function splitTemplate(text: string, token: string): [string, string] {
	const index = text.indexOf(token);
	if (index === -1) return [text, ''];
	return [text.slice(0, index), text.slice(index + token.length)];
}

function yearlySavingsPercent(monthlyMinor: number | null | undefined, yearlyMinor: number | null | undefined) {
	if (monthlyMinor == null || yearlyMinor == null || monthlyMinor <= 0) return null;
	const percent = Math.round((1 - yearlyMinor / (monthlyMinor * 12)) * 100);
	return percent > 0 ? percent : null;
}

interface PageButtonProps {
	variant: 'primary' | 'secondary';
	onClick: () => void;
	disabled?: boolean;
	loading?: boolean;
	directional?: boolean;
	badge?: string | null;
	children: React.ReactNode;
	flx: string;
}

function PageButton({variant, onClick, disabled, loading, directional, badge, children, flx}: PageButtonProps) {
	return (
		<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page.page-button.focus-ring">
			<button
				type="button"
				className={clsx(styles.button, variant === 'primary' ? styles.buttonPrimary : styles.buttonSecondary)}
				onClick={onClick}
				disabled={disabled || loading}
				aria-busy={loading || undefined}
				data-flx={flx}
			>
				<span data-flx="premium.plutonium-page.plutonium-page.page-button.span">{children}</span>
				{badge && (
					<span
						className={styles.buttonBadge}
						data-flx="premium.plutonium-page.plutonium-page.page-button.button-badge"
					>
						{badge}
					</span>
				)}
				{loading ? (
					<span
						className={styles.buttonSpinner}
						aria-hidden="true"
						data-flx="premium.plutonium-page.plutonium-page.page-button.button-spinner"
					/>
				) : (
					directional && (
						<PlutoniumPageIcon
							name="arrowRight"
							className={clsx(styles.buttonIcon, styles.buttonIconDirectional)}
							data-flx="premium.plutonium-page.plutonium-page.page-button.button-icon"
						/>
					)
				)}
			</button>
		</FocusRing>
	);
}

export const PlutoniumPage = observer(function PlutoniumPage() {
	const PREMIUM_PRODUCT_NAME = getPremiumProductName();
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = PremiumState.loadedForUserId === currentUser?.id ? PremiumState.state : null;
	const locale = LocaleUtils.getCurrentLocale();
	const countryCode = GeoIP.countryCode;
	const isMobile = MobileLayout.enabled;
	const scrollerRef = useRef<HTMLDivElement | null>(null);
	const [pendingPlan, setPendingPlan] = useState<CheckoutPlan | null>(null);
	const guilds = Guilds.getGuilds();
	const visionaryGuild = useMemo(() => guilds.find((guild) => guild.features.has(GuildFeatures.VISIONARY)), [guilds]);
	const subscriptionStatus = useSubscriptionStatus(currentUser, premiumState);
	const {
		priceIds,
		monthlyPrice,
		yearlyPrice,
		currentSubscriptionPrice,
		currentSubscriptionPriceLabel,
		currentSubscriptionListPriceLabel,
		isCurrentSubscriptionGrandfathered,
	} = usePremiumData({premiumState});
	const {
		loadingPortal,
		loadingCancel,
		loadingReactivate,
		loadingEndGrace,
		loadingChangeBillingCycle,
		loadingCancelPendingChange,
		handleOpenCustomerPortal,
		handleCancelSubscription,
		handleEndPremiumGracePeriod,
		handleReactivateSubscription,
		handleChangeSubscriptionBillingCycle,
		handleCancelPendingSubscriptionChange,
	} = useSubscriptionActions(countryCode);
	const {loadingRejoinCommunity, handleCommunityButtonClick} = useCommunityActions(visionaryGuild);
	const {loadingCheckout, handleSelectPlan} = useCheckoutActions(priceIds, countryCode, isMobile, {
		hasGiftTime: subscriptionStatus.isGiftSubscription,
	});
	useEffect(() => {
		if (!currentUser?.id) return;
		void PremiumCommands.refreshPremiumState(countryCode ?? undefined);
	}, [countryCode, currentUser?.id]);
	useEffect(() => {
		if (!loadingCheckout) setPendingPlan(null);
	}, [loadingCheckout]);
	const isClaimed = currentUser?.isClaimed() ?? false;
	const isEmailVerified = currentUser?.verified === true;
	const purchaseDisabled = !isClaimed || !isEmailVerified;
	const userPurchaseDisabled = premiumState?.effective.premium_purchase_disabled === true;
	const purchasesAvailable = arePremiumPurchasesAvailable(userPurchaseDisabled);
	const giftPurchasesAvailable = areGiftPurchasesAvailable(priceIds, userPurchaseDisabled);
	const hasBillingRelationship = subscriptionStatus.hasEverPurchased || premiumState?.billing.subscription != null;
	const billingUnavailable =
		!shouldShowPremiumFeatures() ||
		(!arePremiumPurchasesAvailable() && !(hasBillingRelationship && canServiceStripeSubscriptions()));
	const isMember = subscriptionStatus.shouldShowPremiumCard;
	const canSubscribe = purchasesAvailable && subscriptionStatus.canStartSubscription;
	const savingsPercent = yearlySavingsPercent(priceIds?.monthly_amount_minor, priceIds?.yearly_amount_minor);
	const savingsBadge = savingsPercent != null ? i18n._(SAVE_PERCENT_DESCRIPTOR, {percent: savingsPercent}) : null;
	const uploadSizes = resolveUploadSizes(i18n.locale);
	const purchaseBlockedNote = !isClaimed
		? i18n._(PURCHASE_BLOCKED_CLAIM_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
		: !isEmailVerified
			? i18n._(PURCHASE_BLOCKED_VERIFY_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
			: null;
	const startCheckout = useCallback(
		(plan: CheckoutPlan) => {
			if (purchaseDisabled || loadingCheckout) return;
			setPendingPlan(plan);
			void handleSelectPlan(plan);
		},
		[handleSelectPlan, loadingCheckout, purchaseDisabled],
	);
	const scrollToId = useCallback((id: string, block: ScrollLogicalPosition = 'start') => {
		const target = scrollerRef.current?.querySelector<HTMLElement>(`#${id}`);
		target?.scrollIntoView({behavior: 'smooth', block});
	}, []);
	const handleFootnoteClick = useCallback(
		(event: React.MouseEvent<HTMLAnchorElement>) => {
			event.preventDefault();
			scrollToId(FOOTNOTE_ID, 'center');
		},
		[scrollToId],
	);
	const handleBack = useCallback(() => {
		const history = RouterUtils.getHistory();
		if (history && window.history.length > 1) {
			window.history.back();
			return;
		}
		RouterUtils.transitionTo(Routes.ME);
	}, []);
	const handleCancelSubscriptionConfirmed = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={<Trans>Cancel subscription?</Trans>}
					description={
						<Trans>
							You keep your perks until your next renewal date. Reactivate before then to keep your subscriber history.
						</Trans>
					}
					primaryText={<Trans>Cancel subscription</Trans>}
					primaryVariant="danger"
					secondaryText={<Trans>Keep subscription</Trans>}
					onPrimary={async () => {
						await handleCancelSubscription();
					}}
					data-flx="premium.plutonium-page.cancel-subscription.confirm-modal"
				/>
			)),
		);
	}, [handleCancelSubscription]);
	const handleEndPremiumGracePeriodConfirmed = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={<Trans>End grace period now?</Trans>}
					description={
						<Trans>
							You lose all {PREMIUM_PRODUCT_NAME} perks immediately and your subscriber history resets. This cannot be
							undone.
						</Trans>
					}
					primaryText={<Trans>End grace period</Trans>}
					primaryVariant="danger"
					secondaryText={<Trans>Keep grace period</Trans>}
					onPrimary={async () => {
						await handleEndPremiumGracePeriod();
					}}
					data-flx="premium.plutonium-page.end-grace-period.confirm-modal"
				/>
			)),
		);
	}, [handleEndPremiumGracePeriod]);
	if (!currentUser) return null;
	const monthlyLabel = i18n._(MONTHLY_PRICE_DESCRIPTOR, {monthlyPrice});
	const yearlyLabel = i18n._(YEARLY_PRICE_DESCRIPTOR, {yearlyPrice});
	const pricesLoaded = monthlyPrice !== PLACEHOLDER_PRICE && yearlyPrice !== PLACEHOLDER_PRICE;
	const subscribeButtons = (flxSuffix: string) => (
		<>
			<PageButton
				variant="primary"
				onClick={() => startCheckout('yearly')}
				disabled={purchaseDisabled}
				loading={loadingCheckout && pendingPlan === 'yearly'}
				badge={savingsBadge}
				directional
				flx={`premium.plutonium-page.${flxSuffix}.subscribe-yearly`}
				data-flx="premium.plutonium-page.plutonium-page.subscribe-buttons.page-button.start-checkout"
			>
				{i18n._(SUBSCRIBE_YEARLY_DESCRIPTOR)}
			</PageButton>
			<PageButton
				variant="secondary"
				onClick={() => startCheckout('monthly')}
				disabled={purchaseDisabled}
				loading={loadingCheckout && pendingPlan === 'monthly'}
				flx={`premium.plutonium-page.${flxSuffix}.subscribe-monthly`}
				data-flx="premium.plutonium-page.plutonium-page.subscribe-buttons.page-button.start-checkout--2"
			>
				{i18n._(SUBSCRIBE_MONTHLY_DESCRIPTOR)}
			</PageButton>
		</>
	);
	const giftButton = (variant: 'primary' | 'secondary', flxSuffix: string) =>
		giftPurchasesAvailable ? (
			<PageButton
				variant={variant}
				onClick={PlutoniumPageCommands.openGiftPlutoniumModal}
				flx={`premium.plutonium-page.${flxSuffix}.gift`}
				data-flx="premium.plutonium-page.plutonium-page.gift-button.page-button.open-gift-plutonium-modal"
			>
				{i18n._(GIFT_PLUTONIUM_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
			</PageButton>
		) : null;
	const memberLead = (() => {
		if (subscriptionStatus.isVisionary) return i18n._(VISIONARY_THANKS_DESCRIPTOR);
		if (subscriptionStatus.gracePeriodInfo.showExpiredState) {
			return i18n._(LAPSED_DESCRIPTOR, {premiumProductName: getPremiumProductName()});
		}
		if (subscriptionStatus.isGiftGrace) {
			return i18n._(GIFT_GRACE_DESCRIPTOR, {premiumProductName: getPremiumProductName()});
		}
		if (subscriptionStatus.gracePeriodInfo.isInGracePeriod) return i18n._(GRACE_DESCRIPTOR);
		if (subscriptionStatus.isGiftSubscription && subscriptionStatus.actualPremiumUntil) {
			return i18n._(GIFTED_THANKS_DESCRIPTOR, {
				premiumProductName: getPremiumProductName(),
				date: getFormattedLongDate(subscriptionStatus.actualPremiumUntil, locale),
			});
		}
		return i18n._(MEMBER_THANKS_DESCRIPTOR, {premiumProductName: getPremiumProductName()});
	})();
	const closingTitle = !canSubscribe
		? i18n._(CLOSING_MEMBER_TITLE_DESCRIPTOR)
		: subscriptionStatus.isGiftSubscription
			? i18n._(CLOSING_GIFT_TITLE_DESCRIPTOR)
			: i18n._(CLOSING_TITLE_DESCRIPTOR);
	const closingBody = !canSubscribe
		? i18n._(CLOSING_MEMBER_BODY_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
		: subscriptionStatus.isGiftSubscription
			? i18n._(CLOSING_GIFT_BODY_DESCRIPTOR)
			: i18n._(CLOSING_BODY_DESCRIPTOR);
	const showDonationHint = !RuntimeConfig.isSelfHosted();
	const donationTemplate = i18n._(DONATION_HINT_DESCRIPTOR, {productName: PRODUCT_NAME, donateLink: '\u0000'});
	const [donationBefore, donationAfter] = splitTemplate(donationTemplate, '\u0000');
	const footnoteTemplate = i18n._(TAG_FOOTNOTE_DESCRIPTOR, {productName: PRODUCT_NAME, visionaryLink: '\u0000'});
	const [footnoteBefore, footnoteAfter] = splitTemplate(footnoteTemplate, '\u0000');
	const secondaryLinks = (
		<div className={styles.secondaryLinks} data-flx="premium.plutonium-page.hero.secondary-links">
			{giftPurchasesAvailable && (
				<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page.focus-ring">
					<button
						type="button"
						className={styles.textButton}
						onClick={PlutoniumPageCommands.openGiftPlutoniumModal}
						data-flx="premium.plutonium-page.hero.gift-link"
					>
						<GiftIcon
							weight="fill"
							className={styles.textButtonIcon}
							aria-hidden="true"
							data-flx="premium.plutonium-page.plutonium-page.text-button-icon"
						/>
						<span data-flx="premium.plutonium-page.plutonium-page.span">
							{i18n._(GIFT_PLUTONIUM_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
						</span>
					</button>
				</FocusRing>
			)}
			<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page.focus-ring--2">
				<button
					type="button"
					className={styles.textButton}
					onClick={PlutoniumPageCommands.openGiftInventorySettings}
					data-flx="premium.plutonium-page.hero.redeem-link"
				>
					<TicketIcon
						weight="fill"
						className={styles.textButtonIcon}
						aria-hidden="true"
						data-flx="premium.plutonium-page.plutonium-page.text-button-icon--2"
					/>
					<span data-flx="premium.plutonium-page.plutonium-page.span--2">{i18n._(REDEEM_GIFT_CODE_DESCRIPTOR)}</span>
				</button>
			</FocusRing>
		</div>
	);
	const renderPurchaseHero = (lead: string | null) => (
		<>
			{lead && (
				<p className={styles.heroLead} data-flx="premium.plutonium-page.hero.lead">
					{lead}
				</p>
			)}
			{pricesLoaded && (
				<p className={styles.priceLine} data-flx="premium.plutonium-page.hero.price-line">
					<span className={styles.price} data-flx="premium.plutonium-page.plutonium-page.render-purchase-hero.price">
						{monthlyLabel}
					</span>
					<span
						className={styles.priceOr}
						data-flx="premium.plutonium-page.plutonium-page.render-purchase-hero.price-or"
					>
						{i18n._(PRICE_OR_DESCRIPTOR)}
					</span>
					<span className={styles.price} data-flx="premium.plutonium-page.plutonium-page.render-purchase-hero.price--2">
						{yearlyLabel}
					</span>
				</p>
			)}
			<div className={clsx(styles.actions, styles.heroActions)} data-flx="premium.plutonium-page.hero.actions">
				{subscribeButtons('hero')}
			</div>
			{purchaseBlockedNote && (
				<p className={styles.notice} role="note" data-flx="premium.plutonium-page.hero.purchase-blocked">
					{purchaseBlockedNote}
				</p>
			)}
			{secondaryLinks}
		</>
	);
	const renderHeroBody = () => {
		if (isMember && canSubscribe) return renderPurchaseHero(memberLead);
		if (isMember) {
			return (
				<>
					<p className={styles.heroLead} data-flx="premium.plutonium-page.hero.lead">
						{memberLead}
					</p>
					<div className={clsx(styles.actions, styles.heroActions)} data-flx="premium.plutonium-page.hero.actions">
						<PageButton
							variant="primary"
							onClick={() => scrollToId(MANAGE_ID)}
							directional
							flx="premium.plutonium-page.hero.manage"
							data-flx="premium.plutonium-page.plutonium-page.render-hero-body.page-button.scroll-to-id"
						>
							{i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
						</PageButton>
						{giftButton('secondary', 'hero')}
					</div>
				</>
			);
		}
		if (!purchasesAvailable) {
			return (
				<>
					<p className={styles.heroLead} data-flx="premium.plutonium-page.hero.lead">
						{i18n._(REDEEM_ONLY_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
					</p>
					<div className={clsx(styles.actions, styles.heroActions)} data-flx="premium.plutonium-page.hero.actions">
						<PageButton
							variant="primary"
							onClick={PlutoniumPageCommands.openGiftInventorySettings}
							directional
							flx="premium.plutonium-page.hero.redeem"
							data-flx="premium.plutonium-page.plutonium-page.render-hero-body.page-button.open-gift-inventory-settings"
						>
							{i18n._(REDEEM_GIFT_CODE_DESCRIPTOR)}
						</PageButton>
					</div>
				</>
			);
		}
		return renderPurchaseHero(null);
	};
	return (
		<div
			className={styles.root}
			role="main"
			aria-label={i18n._(PAGE_LABEL_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
			data-flx="premium.plutonium-page.root"
		>
			<div ref={scrollerRef} className={styles.scroller} data-flx="premium.plutonium-page.scroller">
				<div
					className={styles.starfield}
					aria-hidden="true"
					data-flx="premium.plutonium-page.plutonium-page.starfield"
				/>
				{isMobile && (
					<div className={styles.mobileBar} data-flx="premium.plutonium-page.mobile-bar">
						<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page.focus-ring--3">
							<button
								type="button"
								className={styles.backButton}
								onClick={handleBack}
								aria-label={i18n._(BACK_DESCRIPTOR)}
								data-flx="premium.plutonium-page.mobile-bar.back"
							>
								<ArrowLeftIcon
									weight="bold"
									className={styles.backIcon}
									aria-hidden="true"
									data-flx="premium.plutonium-page.plutonium-page.back-icon"
								/>
							</button>
						</FocusRing>
					</div>
				)}
				<section className={styles.hero} data-flx="premium.plutonium-page.hero">
					<div className={styles.heroInner} data-flx="premium.plutonium-page.plutonium-page.hero-inner">
						{!RuntimeConfig.isSelfHosted() && (
							<div className={styles.crownWrap} data-flx="premium.plutonium-page.plutonium-page.crown-wrap">
								<picture data-flx="premium.plutonium-page.plutonium-page.picture">
									<source
										type="image/avif"
										srcSet={CROWN_AVIF}
										data-flx="premium.plutonium-page.plutonium-page.source.image-avif"
									/>
									<img
										draggable={false}
										className={styles.crown}
										src={CROWN_WEBP}
										width={480}
										height={395}
										alt=""
										decoding="sync"
										fetchPriority="high"
										data-flx="premium.plutonium-page.hero.crown"
									/>
								</picture>
							</div>
						)}
						<h1 className={styles.displayHeading} data-flx="premium.plutonium-page.hero.title">
							{getPremiumProductFullName()}
						</h1>
						{renderHeroBody()}
						{showDonationHint && (
							<p className={styles.meta} data-flx="premium.plutonium-page.hero.donation-hint">
								<span data-flx="premium.plutonium-page.plutonium-page.span--3">{donationBefore}</span>
								<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page.focus-ring--4">
									<a
										className={styles.inlineLink}
										href={marketingUrl('donate')}
										target="_blank"
										rel="noopener noreferrer"
										data-flx="premium.plutonium-page.hero.donation-link"
									>
										{i18n._(DONATE_INSTEAD_DESCRIPTOR)}
									</a>
								</FocusRing>
								<span data-flx="premium.plutonium-page.plutonium-page.span--4">{donationAfter}</span>
							</p>
						)}
					</div>
				</section>
				<div className={styles.sections} data-flx="premium.plutonium-page.sections">
					{isMember && (
						<section
							id={MANAGE_ID}
							className={clsx(styles.glassPanel, styles.managePanel)}
							aria-labelledby={`${MANAGE_ID}-heading`}
							data-flx="premium.plutonium-page.manage"
						>
							<h2
								id={`${MANAGE_ID}-heading`}
								className={styles.manageHeading}
								data-flx="premium.plutonium-page.plutonium-page.manage-heading"
							>
								{i18n._(MANAGE_HEADING_DESCRIPTOR)}
							</h2>
							<div className={styles.manageSurface} data-flx="premium.plutonium-page.manage.surface">
								<SubscriptionCard
									locale={locale}
									isVisionary={subscriptionStatus.isVisionary}
									perksDisabled={subscriptionStatus.perksDisabled}
									isGiftSubscription={subscriptionStatus.isGiftSubscription}
									isGiftGrace={subscriptionStatus.isGiftGrace}
									canSubscribe={canSubscribe}
									storeSubscription={subscriptionStatus.storeSubscription}
									premiumUntil={subscriptionStatus.actualPremiumUntil}
									billingCycle={subscriptionStatus.billingCycle}
									monthlyPrice={monthlyPrice}
									yearlyPrice={yearlyPrice}
									monthlyAmountMinor={priceIds?.monthly_amount_minor ?? null}
									yearlyAmountMinor={priceIds?.yearly_amount_minor ?? null}
									priceCurrency={priceIds?.currency ?? null}
									currentSubscriptionAmountMinor={currentSubscriptionPrice?.amount_minor ?? null}
									currentSubscriptionCurrency={currentSubscriptionPrice?.currency ?? null}
									currentSubscriptionPriceLabel={currentSubscriptionPriceLabel}
									currentSubscriptionListPriceLabel={currentSubscriptionListPriceLabel}
									isCurrentSubscriptionGrandfathered={isCurrentSubscriptionGrandfathered}
									pendingSubscriptionChange={premiumState?.billing.pending_subscription_change ?? null}
									gracePeriodInfo={subscriptionStatus.gracePeriodInfo}
									premiumWillCancel={subscriptionStatus.premiumWillCancel}
									hasEverPurchased={subscriptionStatus.hasEverPurchased}
									shouldUseCancelQuickAction={subscriptionStatus.shouldUseCancelQuickAction}
									shouldUseReactivateQuickAction={subscriptionStatus.shouldUseReactivateQuickAction}
									shouldUseChangePlanQuickAction={subscriptionStatus.shouldUseChangePlanQuickAction}
									loadingPortal={loadingPortal}
									loadingCancel={loadingCancel}
									loadingReactivate={loadingReactivate}
									loadingEndGrace={loadingEndGrace}
									loadingChangeBillingCycle={loadingChangeBillingCycle}
									loadingCancelPendingChange={loadingCancelPendingChange}
									loadingRejoinCommunity={loadingRejoinCommunity}
									scrollToPerks={() => scrollToId(COMPARE_ID)}
									navigateToRedeemGift={PlutoniumPageCommands.openGiftInventorySettings}
									handleStartSubscription={() => scrollToId(SUBSCRIBE_ID, 'center')}
									handleOpenCustomerPortal={handleOpenCustomerPortal}
									handleReactivateSubscription={handleReactivateSubscription}
									handleCancelSubscription={handleCancelSubscriptionConfirmed}
									handleEndPremiumGracePeriod={handleEndPremiumGracePeriodConfirmed}
									handleChangeSubscriptionBillingCycle={handleChangeSubscriptionBillingCycle}
									handleCancelPendingSubscriptionChange={handleCancelPendingSubscriptionChange}
									handleCommunityButtonClick={handleCommunityButtonClick}
									purchaseDisabled={purchaseDisabled}
									purchaseDisabledTooltip={purchaseBlockedNote ?? undefined}
									billingUnavailable={billingUnavailable}
									data-flx="premium.plutonium-page.manage.subscription-card"
								/>
								{purchasesAvailable && (
									<PurchaseDisclaimer
										align="center"
										isPremium
										data-flx="premium.plutonium-page.plutonium-page.purchase-disclaimer"
									/>
								)}
								{subscriptionStatus.hasEverPurchased &&
									!billingUnavailable &&
									premiumState?.billing.stripe_customer_id != null && (
										<>
											<PurchaseHistorySection
												premiumState={premiumState}
												loadingPortal={loadingPortal}
												handleOpenCustomerPortal={handleOpenCustomerPortal}
												data-flx="premium.plutonium-page.manage.purchase-history"
											/>
											{!RuntimeConfig.isSelfHosted() && (
												<SelfServeRefundSection
													eligibility={premiumState?.billing.refund_eligibility ?? null}
													refreshPremiumState={() => PremiumCommands.refreshPremiumState(countryCode ?? undefined)}
													data-flx="premium.plutonium-page.manage.self-serve-refund"
												/>
											)}
										</>
									)}
							</div>
						</section>
					)}
					<PlutoniumPageShowcase
						uploadSize={uploadSizes.premium}
						freeUploadSize={uploadSizes.free}
						footnoteId={FOOTNOTE_ID}
						onFootnoteClick={handleFootnoteClick}
						data-flx="premium.plutonium-page.plutonium-page.plutonium-page-showcase"
					/>
					<div id={COMPARE_ID} data-flx="premium.plutonium-page.compare-anchor">
						<PlutoniumPageComparison
							footnoteId={FOOTNOTE_ID}
							onFootnoteClick={handleFootnoteClick}
							actions={
								canSubscribe ? subscribeButtons('comparison') : isMember ? giftButton('primary', 'comparison') : null
							}
							data-flx="premium.plutonium-page.plutonium-page.plutonium-page-comparison"
						/>
					</div>
					{(canSubscribe || (isMember && giftPurchasesAvailable)) && (
						<section
							id={SUBSCRIBE_ID}
							className={clsx(styles.glassPanel, styles.glassPanelBrand, styles.closing)}
							data-flx="premium.plutonium-page.closing"
						>
							<div className={styles.closingInner} data-flx="premium.plutonium-page.plutonium-page.closing-inner">
								<h2 className={styles.displayHeading} data-flx="premium.plutonium-page.closing.title">
									{closingTitle}
								</h2>
								<p className={styles.closingBody} data-flx="premium.plutonium-page.closing.body">
									{closingBody}
								</p>
								<div
									className={clsx(styles.actions, styles.closingActions)}
									data-flx="premium.plutonium-page.closing.actions"
								>
									{canSubscribe ? subscribeButtons('closing') : giftButton('primary', 'closing')}
								</div>
							</div>
						</section>
					)}
					{!RuntimeConfig.usesUniqueUsernames && (
						<p id={FOOTNOTE_ID} className={styles.footnote} data-flx="premium.plutonium-page.footnote">
							<span data-flx="premium.plutonium-page.plutonium-page.span--5">{`* ${footnoteBefore}`}</span>
							<FocusRing offset={-2} data-flx="premium.plutonium-page.plutonium-page.focus-ring--5">
								<a
									className={styles.inlineLink}
									href={Routes.helpArticle('visionary')}
									target="_blank"
									rel="noopener noreferrer"
									data-flx="premium.plutonium-page.footnote.visionary-link"
								>
									{i18n._(TAG_FOOTNOTE_LINK_DESCRIPTOR)}
								</a>
							</FocusRing>
							<span data-flx="premium.plutonium-page.plutonium-page.span--6">{footnoteAfter}</span>
						</p>
					)}
				</div>
			</div>
		</div>
	);
});
