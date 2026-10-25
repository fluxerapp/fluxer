// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {EmailVerificationAlert} from '@app/features/app/components/dialogs/components/EmailVerificationAlert';
import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	OPERATOR_PASS_NAME,
	PAYMENT_PROVIDER_NAME,
	PRODUCT_LABS_COMMUNITY_NAME,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {isolateBidi, isolateChannelNames} from '@app/features/i18n/utils/BidiIsolate';
import {CANCEL_DESCRIPTOR, CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import {
	AGE_BOX_ID,
	OperatorPassCheckoutDraft,
	OperatorPassCheckoutForm,
	WAIVER_BOX_ID,
	WALL_NAME_FIELD_ID,
} from '@app/features/premium/components/modals/OperatorPassCheckoutForm';
import styles from '@app/features/premium/components/modals/OperatorPassCheckoutModal.module.css';
import {
	renderOperatorPassHolderCard,
	renderOperatorPassPriceBlock,
} from '@app/features/premium/components/modals/OperatorPassCheckoutPresentation';
import {
	clampOperatorPassQuantity,
	focusFirstInvalidOperatorPassField,
	focusOperatorPassField,
	getOperatorPassErrorCode,
	getOperatorPassWallNameProblem,
	normalizeOperatorPassWallName,
	OPERATOR_PASS_LABS_INVITE_CODE,
} from '@app/features/premium/utils/OperatorPassUtils';
import {recordPremiumCheckoutReturnIntent} from '@app/features/premium/utils/PremiumCheckoutReturnIntent';
import {
	OPERATOR_PASS_CHARACTERISTICS_DESCRIPTOR,
	OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR,
	OPERATOR_PASS_OPEN_LABS_DESCRIPTOR,
	OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {getPremiumPurchaseBlockedReason} from '@app/features/premium/utils/PremiumPurchaseBlock';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {type SegmentedTab, SegmentedTabs} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import Users from '@app/features/user/state/Users';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {AccountVerificationNeeds} from '@fluxer/constants/src/AccountTierConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import {
	OPERATOR_PASS_CURRENCIES,
	OPERATOR_PASS_PHASES,
	type OperatorPassCurrency,
	type OperatorPassPhase,
	type OperatorPassPriceChangedErrorData,
	type OperatorPassPurchaseBlockedReason,
	type OperatorPassStateResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MinusIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

type CheckoutLoadState =
	| {status: 'loading'; value: null}
	| {status: 'ready'; value: OperatorPassStateResponse}
	| {status: 'failed'; value: OperatorPassStateResponse | null};

type Notice =
	| {kind: 'canceled'}
	| {kind: 'price_changed'; price: string}
	| {kind: 'not_on_sale'}
	| {kind: 'country_unavailable'}
	| {kind: 'email_unverified'}
	| {kind: 'unclaimed'}
	| {kind: 'purchase_disabled'}
	| {kind: 'payment_dispute_open'}
	| {kind: 'removed'}
	| {kind: 'rate_limited'}
	| {kind: 'failed'};

const NO_DONATION_TAB_ID = 'none';

const BONUS_DESCRIPTOR = msg({
	message: 'While we run it, you also get the Operator role and #operators in {labsName}.',
	comment:
		'Line in the Operator Pass checkout dialog under what the pass gives. {labsName} is the community name and it in while we run it means that community. Operator is the role name and #operators is a channel name, keep both as is. This bonus is outside the price, keep the words while we run it.',
});
const WHATS_INCLUDED_DESCRIPTOR = msg({
	message: "What's included",
	comment:
		'Link in the Operator Pass checkout dialog that opens the Operator Pass page on the website, at the part that explains what the pass and the bonus include.',
});
const AGE_CONFIRMATION_DESCRIPTOR = msg({
	message: "I'm 18 or older, or have a parent's or guardian's permission.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying more than one pass. The buyer confirms their age or that a parent or guardian allows the purchase.',
});
const QUANTITY_LABEL_DESCRIPTOR = msg({
	message: 'Number of passes',
	comment: 'Label of the control that sets how many Operator Passes to buy. All of them go to your own account.',
});
const QUANTITY_HINT_DESCRIPTOR = msg({
	message: 'More passes show larger and earlier on the wall.',
	comment:
		"Hint under the number of Operator Passes to buy. The wall is the Operators' wall, the list of Operators on the homepage. A neutral fact, never translate it as an encouragement to buy more.",
});
const QUANTITY_CAP_DESCRIPTOR = msg({
	message: 'Up to {max} per purchase.',
	comment:
		'Note under the Operator Pass quantity selector when the buyer reached the highest number of passes one purchase allows. {max} is that number, such as 25.',
});
const DECREASE_QUANTITY_DESCRIPTOR = msg({
	message: 'One pass fewer',
	comment: 'Accessible label of the button that lowers the number of Operator Passes to buy by one.',
});
const INCREASE_QUANTITY_DESCRIPTOR = msg({
	message: 'One pass more',
	comment: 'Accessible label of the button that raises the number of Operator Passes to buy by one.',
});
const WAITLIST_DESCRIPTION_DESCRIPTOR = msg({
	message: "We'll email you when it opens.",
	comment:
		'Line in the Operator Pass dialog before the pass can be bought, under the button that opens the waitlist. It means the sale of the Operator Pass opens.',
});
const JOIN_WAITLIST_DESCRIPTOR = msg({
	message: 'Join the waitlist',
	comment: 'Button in the Operator Pass dialog before the pass can be bought. Opens the waitlist on the website.',
});
const OWNED_BODY_DESCRIPTOR = msg({
	message: 'Thanks for backing {productName}. Your passes are in your profile settings.',
	comment:
		'Body in the Operator Pass dialog when the account already has a pass and buying is not open. Backing means supporting the project.',
});
const WAIVER_DESCRIPTOR = msg({
	message:
		"Deliver my pass now. I understand I lose my right to withdraw once it's delivered, and that the bonus isn't part of the price.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying one pass. The buyer asks for delivery right after payment. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal, never taking money out.',
});
const WAIVER_MULTIPLE_DESCRIPTOR = msg({
	message:
		"Deliver all {count, plural, one {# pass} other {# passes}} now. I understand I lose my right to withdraw once they're delivered, and that the bonus isn't part of the price.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying more than one pass. {count} is the number of passes, 2 or more. The buyer asks for delivery right after payment. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal, never taking money out.',
});
const WAIVER_COUNTRY_DESCRIPTOR = msg({
	message:
		"Deliver my pass now. I can still cancel within {days, plural, one {# day} other {# days}} for a full refund. The bonus isn't part of the price.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying one pass in a country whose withdrawal right cannot be waived, such as Brazil. {days} is the number of days after payment. Cancel means using that right for a full refund (pt-BR: desistir). Consumer law text, translate faithfully and keep every condition.',
});
const WAIVER_COUNTRY_MULTIPLE_DESCRIPTOR = msg({
	message:
		"Deliver all {count, plural, one {# pass} other {# passes}} now. I can still cancel each one within {days, plural, one {# day} other {# days}} for a full refund. The bonus isn't part of the price.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying more than one pass in a country whose withdrawal right cannot be waived, such as Brazil. {count} is the number of passes, {days} the number of days after payment. Cancel means using that right for a full refund (pt-BR: desistir). Consumer law text, translate faithfully and keep every condition.',
});
const DONATION_ADD_DESCRIPTOR = msg({
	message: 'Add a monthly donation',
	comment:
		'Text button in the Operator Pass checkout dialog that shows the optional monthly donation choice. The donation is separate from the pass.',
});
const DONATION_LABEL_DESCRIPTOR = msg({
	message: 'Monthly donation (optional)',
	comment:
		'Heading of the optional monthly donation choice in the Operator Pass checkout dialog. The donation renews every month until canceled.',
});
const NO_DONATION_DESCRIPTOR = msg({
	message: 'None',
	comment:
		'Option in the Operator Pass checkout dialog to skip the optional monthly donation. Sits next to amounts such as $5.',
});
const DUE_TODAY_DESCRIPTOR = msg({
	message: 'Due today: {total}, then {donation} a month until you cancel.',
	comment:
		'Summary in the Operator Pass checkout dialog when a monthly donation is added. {total} is what is paid today and {donation} the monthly amount, both prices with currency. Cancel means ending the monthly donation.',
});
const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue to payment',
	comment: 'Primary button in the Operator Pass checkout dialog. Opens the payment page.',
});
const RETRY_DESCRIPTOR = msg({
	message: 'Try again',
	comment: 'Button in the Operator Pass checkout dialog to reload the price after a failed load.',
});
const LOAD_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load the price. Try again.",
	comment: 'Error in the Operator Pass checkout dialog when the price could not be loaded.',
});
const CANCELED_NOTICE_DESCRIPTOR = msg({
	message: "Checkout canceled. You weren't charged.",
	comment: 'Notice in the Operator Pass checkout dialog after the buyer canceled on the payment page.',
});
const PRICE_CHANGED_NOTICE_DESCRIPTOR = msg({
	message: 'The price is now {price}.',
	comment:
		'Notice in the Operator Pass checkout dialog when the price changed before payment, usually because the early bird price ended. {price} is a price with currency.',
});
const NOT_ON_SALE_NOTICE_DESCRIPTOR = msg({
	message: 'Not on sale yet.',
	comment:
		'Notice in the Operator Pass checkout dialog when checkout was refused because the Operator Pass cannot be bought yet.',
});
const COUNTRY_UNAVAILABLE_NOTICE_DESCRIPTOR = msg({
	message: "The {operatorPassName} isn't available in your country yet.",
	comment:
		'Notice in the Operator Pass checkout dialog when the pass cannot be bought in the country of the buyer. Operator Pass is a product name.',
});
const EMAIL_UNVERIFIED_NOTICE_DESCRIPTOR = msg({
	message: 'Confirm your email to buy the {operatorPassName}.',
	comment:
		'Notice in the Operator Pass checkout dialog for accounts without a verified email. Operator Pass is a product name.',
});
const UNCLAIMED_NOTICE_DESCRIPTOR = msg({
	message: 'Claim your account to buy the {operatorPassName}.',
	comment:
		'Notice in the Operator Pass checkout dialog for accounts that need a password before they can buy. Operator Pass is a product name.',
});
const NO_EMAIL_NOTICE_DESCRIPTOR = msg({
	message: 'Add an email to your account to buy the {operatorPassName}.',
	comment:
		'Notice in the Operator Pass checkout dialog for accounts that have no email yet. Operator Pass is a product name.',
});
const REMOVED_NOTICE_DESCRIPTOR = msg({
	message: "This account can't buy the {operatorPassName} again.",
	comment:
		'Notice in the Operator Pass checkout dialog when the pass was removed from the account permanently. Operator Pass is a product name and stays untranslated.',
});
const PAYMENT_DISPUTE_NOTICE_DESCRIPTOR = msg({
	message: 'Purchases are paused while a payment dispute is open.',
	comment:
		'Notice in the Operator Pass checkout dialog while a payment dispute this account made is open. A payment dispute is a chargeback filed with a bank or card company. Neutral tone, never suggest the person did something wrong.',
});
const RATE_LIMITED_NOTICE_DESCRIPTOR = msg({
	message: 'Too many tries. Wait a minute and try again.',
	comment: 'Notice in the Operator Pass checkout dialog when checkout was rate limited.',
});
const FAILED_NOTICE_DESCRIPTOR = msg({
	message: "Couldn't start checkout. Try again.",
	comment: 'Notice in the Operator Pass checkout dialog when checkout failed for an unknown reason.',
});
const COMPLETE_PAYMENT_TITLE_DESCRIPTOR = msg({
	message: 'Complete payment',
	comment: 'Title of the confirmation shown on mobile before opening the payment page for the Operator Pass.',
});
const COMPLETE_PAYMENT_BODY_DESCRIPTOR = msg({
	message: 'Finish paying on {paymentProviderName}, then come back to {productName}.',
	comment:
		'Body of the confirmation shown on mobile before opening the payment page for the Operator Pass. {paymentProviderName} is the payment processor name.',
});
const COMPLETE_PAYMENT_BUTTON_DESCRIPTOR = msg({
	message: 'Continue to {paymentProviderName}',
	comment:
		'Button in the confirmation shown on mobile before opening the payment page for the Operator Pass. {paymentProviderName} is the payment processor name.',
});

const BLOCKING_NOTICE_KINDS: ReadonlySet<Notice['kind']> = new Set([
	'country_unavailable',
	'email_unverified',
	'unclaimed',
	'purchase_disabled',
	'payment_dispute_open',
	'removed',
]);

const logger = new Logger('OperatorPassCheckoutModal');

function isPhase(value: unknown): value is OperatorPassPhase {
	return OPERATOR_PASS_PHASES.some((phase) => phase === value);
}

function isCurrency(value: unknown): value is OperatorPassCurrency {
	return OPERATOR_PASS_CURRENCIES.some((currency) => currency === value);
}

function priceChangedData(error: unknown): OperatorPassPriceChangedErrorData | null {
	if (!(error instanceof HttpError)) return null;
	const body = error.body as Record<string, unknown> | null | undefined;
	if (
		!body ||
		typeof body.price_minor !== 'number' ||
		typeof body.quote !== 'string' ||
		!isCurrency(body.currency) ||
		!isPhase(body.phase)
	) {
		return null;
	}
	return {phase: body.phase, currency: body.currency, price_minor: body.price_minor, quote: body.quote};
}

function noticeForBlockedReason(reason: OperatorPassPurchaseBlockedReason | null): Notice | null {
	switch (reason) {
		case 'country_unavailable':
			return {kind: 'country_unavailable'};
		case 'email_unverified':
			return {kind: 'email_unverified'};
		case 'unclaimed':
			return {kind: 'unclaimed'};
		case 'purchase_disabled':
			return {kind: 'purchase_disabled'};
		case 'payment_dispute_open':
			return {kind: 'payment_dispute_open'};
		case 'removed':
			return {kind: 'removed'};
		default:
			return null;
	}
}

interface OperatorPassCheckoutModalProps {
	canceled: boolean;
}

export const OperatorPassCheckoutModal = observer(function OperatorPassCheckoutModal({
	canceled,
}: OperatorPassCheckoutModalProps) {
	const {i18n} = useLingui();
	const [loadState, setLoadState] = useState<CheckoutLoadState>({status: 'loading', value: null});
	const state = loadState.value;
	const [quantity, setQuantity] = useState(1);
	const [donationOpen, setDonationOpen] = useState(false);
	const donationRef = useRef<HTMLDivElement | null>(null);
	useLayoutEffect(() => {
		if (!donationOpen) return;
		donationRef.current?.querySelector<HTMLElement>('[role="radio"][tabindex="0"]')?.focus();
	}, [donationOpen]);
	const [donationAmountMinor, setDonationAmountMinor] = useState<number | null>(null);
	const [draft] = useState(() => new OperatorPassCheckoutDraft(Users.currentUser?.displayName ?? ''));
	const {
		wallName,
		wallOptOut,
		wallNameProblem,
		waiver,
		ageConfirmed,
		waiverMissing,
		ageMissing,
		revealWallField,
		setWallNameProblem,
		setWaiver,
		setAgeConfirmed,
		setWaiverMissing,
		setAgeMissing,
		setRevealWallField,
		resetConsents,
	} = draft;
	const [submitting, setSubmitting] = useState(false);
	const [notice, setNotice] = useState<Notice | null>(canceled ? {kind: 'canceled'} : null);
	const noticeRef = useRef<HTMLDivElement | null>(null);
	const generationRef = useRef(0);
	const mountedRef = useRef(true);
	useEffect(() => {
		mountedRef.current = true;
		return () => {
			mountedRef.current = false;
		};
	}, []);
	const load = useCallback(async () => {
		const generation = ++generationRef.current;
		setLoadState((current) => {
			if (current.status !== 'failed') return current;
			if (current.value === null) return {status: 'loading', value: null};
			return {status: 'ready', value: current.value};
		});
		try {
			const next = await OperatorPassCommands.fetchState();
			if (!mountedRef.current || generation !== generationRef.current) return;
			setLoadState({status: 'ready', value: next});
			setQuantity((current) => clampOperatorPassQuantity(current, next.max_quantity));
		} catch (error) {
			logger.error('Failed to load Operator Pass state', error);
			if (!mountedRef.current || generation !== generationRef.current) return;
			setLoadState((current) => {
				if (current.status === 'failed') return current;
				return {status: 'failed', value: current.value};
			});
		}
	}, []);
	useEffect(() => {
		void load();
	}, [load]);
	const accountTier = Users.currentUser?.accountTier ?? null;
	const accountTierRef = useRef(accountTier);
	useEffect(() => {
		if (accountTierRef.current === accountTier) return;
		accountTierRef.current = accountTier;
		setNotice((current) => (current?.kind === 'email_unverified' || current?.kind === 'unclaimed' ? null : current));
		void load();
	}, [accountTier, load]);
	const locale = getCurrentLocale();
	const formatPrice = useCallback(
		(amountMinor: number, currency: string) =>
			formatMinorUnitPrice(amountMinor, currency, locale, OperatorPassCommands.getCountryCode()),
		[locale],
	);
	const blockedReason = state?.purchase_blocked_reason ?? null;
	const onSale = state != null && state.phase !== 'waitlist' && blockedReason !== 'not_on_sale';
	const operator = state?.holder ?? null;
	const asksWallChoice = !operator?.wall_chosen || revealWallField;
	const boxTermsKey = state
		? `${state.currency}:${state.withdrawal_rule?.country ?? ''}:${state.withdrawal_rule?.days ?? ''}`
		: null;
	const boxTermsKeyRef = useRef(boxTermsKey);
	useEffect(() => {
		if (boxTermsKeyRef.current === boxTermsKey) return;
		boxTermsKeyRef.current = boxTermsKey;
		resetConsents();
	}, [boxTermsKey, resetConsents]);
	const blockingNotice = onSale
		? (noticeForBlockedReason(blockedReason) ?? (notice && BLOCKING_NOTICE_KINDS.has(notice.kind) ? notice : null))
		: null;
	const topNotice = blockingNotice ?? notice;
	const topNoticeKind = topNotice?.kind ?? null;
	useLayoutEffect(() => {
		if (!topNoticeKind) return;
		noticeRef.current?.scrollIntoView?.({block: 'nearest'});
	}, [topNoticeKind, notice]);
	const donationTabs = useMemo<Array<SegmentedTab<string>>>(() => {
		if (!state) return [];
		return [
			{id: NO_DONATION_TAB_ID, label: i18n._(NO_DONATION_DESCRIPTOR)},
			...state.donation.presets_minor.map((amount) => ({
				id: String(amount),
				label: formatPrice(amount, state.donation.currency),
			})),
		];
	}, [formatPrice, i18n, state]);
	const changeQuantity = (next: number) => {
		if (!state) return;
		const clamped = clampOperatorPassQuantity(next, state.max_quantity);
		if (clamped === quantity) return;
		setQuantity(clamped);
		resetConsents();
	};
	const openLabs = () => {
		void InviteCommands.openAcceptModal(OPERATOR_PASS_LABS_INVITE_CODE, currentInstanceTarget());
	};
	const openWaitlist = () => {
		void openExternalUrl(`${Routes.operatorPassInfo()}#waitlist`);
	};
	const openCheckoutUrl = (url: string) => {
		const launch = () => {
			recordPremiumCheckoutReturnIntent('operator_pass', {passCount: state?.holder?.count ?? 0});
			void openExternalUrl(url);
		};
		if (!MobileLayout.enabled) {
			launch();
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(COMPLETE_PAYMENT_TITLE_DESCRIPTOR)}
					description={i18n._(COMPLETE_PAYMENT_BODY_DESCRIPTOR, {
						paymentProviderName: PAYMENT_PROVIDER_NAME,
						productName: PRODUCT_NAME,
					})}
					primaryText={i18n._(COMPLETE_PAYMENT_BUTTON_DESCRIPTOR, {paymentProviderName: PAYMENT_PROVIDER_NAME})}
					primaryVariant="primary"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={launch}
					data-flx="premium.operator-pass-checkout-modal.open-checkout-url.confirm-modal"
				/>
			)),
		);
	};
	const handleCheckoutError = (error: unknown, current: OperatorPassStateResponse) => {
		logger.error('Operator Pass checkout failed', error);
		switch (getOperatorPassErrorCode(error)) {
			case APIErrorCodes.OPERATOR_PASS_PRICE_CHANGED: {
				const data = priceChangedData(error);
				if (!data) {
					void load();
					setNotice({kind: 'failed'});
					return;
				}
				if (data.currency !== current.currency) {
					setDonationAmountMinor(null);
					void load();
					setNotice({kind: 'price_changed', price: formatPrice(data.price_minor, data.currency)});
					return;
				}
				const next = {
					...current,
					phase: data.phase,
					currency: data.currency,
					price_minor: data.price_minor,
					quote: data.quote,
				};
				setLoadState((previous) => ({
					status: previous.status === 'failed' ? 'failed' : 'ready',
					value: next,
				}));
				setNotice({kind: 'price_changed', price: formatPrice(data.price_minor, data.currency)});
				return;
			}
			case APIErrorCodes.DONATION_AMOUNT_INVALID:
				setDonationAmountMinor(null);
				void load();
				setNotice({kind: 'failed'});
				return;
			case APIErrorCodes.OPERATOR_PASS_WALL_NAME_REQUIRED:
				setRevealWallField(true);
				setWallNameProblem('required');
				focusOperatorPassField(WALL_NAME_FIELD_ID);
				return;
			case APIErrorCodes.OPERATOR_PASS_WAIVER_REQUIRED:
				setWaiver(false);
				setWaiverMissing(true);
				focusOperatorPassField(WAIVER_BOX_ID);
				return;
			case APIErrorCodes.OPERATOR_PASS_AGE_CONFIRMATION_REQUIRED:
				setAgeConfirmed(false);
				setAgeMissing(true);
				focusOperatorPassField(AGE_BOX_ID);
				return;
			case APIErrorCodes.OPERATOR_PASS_NOT_ON_SALE:
				void load();
				setNotice({kind: 'not_on_sale'});
				return;
			case APIErrorCodes.OPERATOR_PASS_COUNTRY_UNAVAILABLE:
				void load();
				setNotice({kind: 'country_unavailable'});
				return;
			case APIErrorCodes.OPERATOR_PASS_REMOVED:
				void load();
				setNotice({kind: 'removed'});
				return;
			case APIErrorCodes.EMAIL_VERIFICATION_REQUIRED:
			case APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED:
				setNotice({kind: 'email_unverified'});
				return;
			case APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES:
				setNotice({kind: 'unclaimed'});
				return;
			case APIErrorCodes.PREMIUM_PURCHASE_BLOCKED: {
				const reason = error instanceof HttpError ? getPremiumPurchaseBlockedReason(error.body) : null;
				setNotice({kind: reason === 'payment_dispute_open' ? 'payment_dispute_open' : 'purchase_disabled'});
				return;
			}
			default:
				setNotice(
					error instanceof HttpError && error.status === HttpStatus.TOO_MANY_REQUESTS
						? {kind: 'rate_limited'}
						: {kind: 'failed'},
				);
		}
	};
	const handleContinue = async () => {
		if (!state || submitting) return;
		const current = state;
		const problem = asksWallChoice ? getOperatorPassWallNameProblem(wallName, wallOptOut) : null;
		const missingAge = quantity > 1 && !ageConfirmed;
		setWallNameProblem(problem);
		setWaiverMissing(!waiver);
		setAgeMissing(missingAge);
		if (
			focusFirstInvalidOperatorPassField([
				{id: WALL_NAME_FIELD_ID, invalid: problem !== null},
				{id: WAIVER_BOX_ID, invalid: !waiver},
				{id: AGE_BOX_ID, invalid: missingAge},
			])
		) {
			return;
		}
		setSubmitting(true);
		setNotice(null);
		try {
			const url = await OperatorPassCommands.createCheckout({
				quantity,
				country_code: OperatorPassCommands.getCountryCode(),
				quote: current.quote,
				donation_amount_minor: donationAmountMinor,
				waiver_consent: true,
				consent_locale: locale,
				...(quantity > 1 ? {age_confirmed: true} : {}),
				...(asksWallChoice
					? {wall_name: wallOptOut ? null : normalizeOperatorPassWallName(wallName), wall_opt_out: wallOptOut}
					: {}),
			});
			if (!mountedRef.current) return;
			openCheckoutUrl(url);
		} catch (error) {
			if (!mountedRef.current) return;
			handleCheckoutError(error, current);
		} finally {
			if (mountedRef.current) setSubmitting(false);
		}
	};
	const renderNotice = (value: Notice) => {
		switch (value.kind) {
			case 'canceled':
				return (
					<Modal.Description role="status" data-flx="premium.operator-pass-checkout-modal.notice">
						{i18n._(CANCELED_NOTICE_DESCRIPTOR)}
					</Modal.Description>
				);
			case 'email_unverified':
				if (RuntimeConfig.emailsEnabled) {
					return (
						<div role="alert" data-flx="premium.operator-pass-checkout-modal.notice-alert">
							<EmailVerificationAlert data-flx="premium.operator-pass-checkout-modal.notice">
								{i18n._(EMAIL_UNVERIFIED_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
							</EmailVerificationAlert>
						</div>
					);
				}
				break;
			case 'purchase_disabled': {
				const supportUrl = Routes.help();
				return (
					<div role="alert" data-flx="premium.operator-pass-checkout-modal.notice-alert">
						<WarningAlert data-flx="premium.operator-pass-checkout-modal.notice">
							{supportUrl ? (
								<Trans comment="Notice in the Operator Pass checkout dialog when purchases are turned off for the account. The linked text opens the support page.">
									Purchases are off for this account.{' '}
									<ExternalLink href={supportUrl} data-flx="premium.operator-pass-checkout-modal.support-link">
										Contact support
									</ExternalLink>
									.
								</Trans>
							) : (
								<Trans comment="Notice in the Operator Pass checkout dialog when purchases are turned off for the account, on an instance without a support page.">
									Purchases are off for this account.
								</Trans>
							)}
						</WarningAlert>
					</div>
				);
			}
		}
		const text = (() => {
			switch (value.kind) {
				case 'price_changed':
					return i18n._(PRICE_CHANGED_NOTICE_DESCRIPTOR, {price: value.price});
				case 'not_on_sale':
					return i18n._(NOT_ON_SALE_NOTICE_DESCRIPTOR);
				case 'country_unavailable':
					return i18n._(COUNTRY_UNAVAILABLE_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME});
				case 'email_unverified':
					return i18n._(EMAIL_UNVERIFIED_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME});
				case 'unclaimed':
					return i18n._(
						Users.currentUser?.verificationNeed === AccountVerificationNeeds.SET_PASSWORD
							? UNCLAIMED_NOTICE_DESCRIPTOR
							: NO_EMAIL_NOTICE_DESCRIPTOR,
						{operatorPassName: OPERATOR_PASS_NAME},
					);
				case 'payment_dispute_open':
					return i18n._(PAYMENT_DISPUTE_NOTICE_DESCRIPTOR);
				case 'removed':
					return i18n._(REMOVED_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME});
				case 'rate_limited':
					return i18n._(RATE_LIMITED_NOTICE_DESCRIPTOR);
				default:
					return i18n._(FAILED_NOTICE_DESCRIPTOR);
			}
		})();
		return (
			<div role="alert" data-flx="premium.operator-pass-checkout-modal.notice-alert">
				<WarningAlert data-flx="premium.operator-pass-checkout-modal.notice">{text}</WarningAlert>
			</div>
		);
	};
	const renderOwned = (current: OperatorPassStateResponse) => (
		<>
			{renderOperatorPassHolderCard({current, i18n})}
			<Modal.Description data-flx="premium.operator-pass-checkout-modal.owned-body">
				{i18n._(OWNED_BODY_DESCRIPTOR, {productName: PRODUCT_NAME})}
			</Modal.Description>
		</>
	);
	const renderWaitlist = (current: OperatorPassStateResponse) => (
		<>
			{renderOperatorPassPriceBlock({current, count: 1, i18n, formatPrice})}
			<Modal.Description data-flx="premium.operator-pass-checkout-modal.waitlist">
				{i18n._(WAITLIST_DESCRIPTION_DESCRIPTOR)}
			</Modal.Description>
		</>
	);
	const renderSummary = () => (
		<div className={styles.summary} data-flx="premium.operator-pass-checkout-modal.summary">
			<p className={styles.characteristics} data-flx="premium.operator-pass-checkout-modal.characteristics">
				{i18n._(OPERATOR_PASS_CHARACTERISTICS_DESCRIPTOR, {productName: PRODUCT_NAME})}
			</p>
			<p className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.bonus">
				{isolateChannelNames(i18n._(BONUS_DESCRIPTOR, {labsName: isolateBidi(PRODUCT_LABS_COMMUNITY_NAME)}))}
			</p>
			<ExternalLink
				href={`${Routes.operatorPassInfo()}#included`}
				className={styles.includedLink}
				data-flx="premium.operator-pass-checkout-modal.included-link"
			>
				{i18n._(WHATS_INCLUDED_DESCRIPTOR)}
			</ExternalLink>
		</div>
	);
	const renderQuantity = (current: OperatorPassStateResponse) => (
		<div className={styles.quantity} data-flx="premium.operator-pass-checkout-modal.quantity">
			<div className={styles.quantityRow} data-flx="premium.operator-pass-checkout-modal.quantity-row">
				<span
					id="operator-pass-checkout-quantity-label"
					className={styles.sectionTitle}
					data-flx="premium.operator-pass-checkout-modal.quantity-label"
				>
					{i18n._(QUANTITY_LABEL_DESCRIPTOR)}
				</span>
				<div
					className={styles.stepper}
					role="group"
					aria-labelledby="operator-pass-checkout-quantity-label"
					data-flx="premium.operator-pass-checkout-modal.stepper"
				>
					<Button
						square
						compact
						variant="secondary"
						icon={<MinusIcon weight="bold" data-flx="premium.operator-pass-checkout-modal.minus-icon" />}
						aria-label={i18n._(DECREASE_QUANTITY_DESCRIPTOR)}
						disabled={submitting || quantity <= 1}
						onClick={() => changeQuantity(quantity - 1)}
						data-flx="premium.operator-pass-checkout-modal.button.decrease"
					/>
					<output
						className={styles.quantityValue}
						aria-live="polite"
						data-flx="premium.operator-pass-checkout-modal.quantity-value"
					>
						{quantity}
					</output>
					<Button
						square
						compact
						variant="secondary"
						icon={<PlusIcon weight="bold" data-flx="premium.operator-pass-checkout-modal.plus-icon" />}
						aria-label={i18n._(INCREASE_QUANTITY_DESCRIPTOR)}
						disabled={submitting || quantity >= current.max_quantity}
						onClick={() => changeQuantity(quantity + 1)}
						data-flx="premium.operator-pass-checkout-modal.button.increase"
					/>
				</div>
			</div>
			<p className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.quantity-hint">
				{i18n._(QUANTITY_HINT_DESCRIPTOR)}
			</p>
			{quantity >= current.max_quantity && (
				<p className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.quantity-cap">
					{i18n._(QUANTITY_CAP_DESCRIPTOR, {max: current.max_quantity})}
				</p>
			)}
		</div>
	);
	const waiverText = (current: OperatorPassStateResponse) => {
		const rule = current.withdrawal_rule;
		if (rule) {
			return quantity > 1
				? i18n._(WAIVER_COUNTRY_MULTIPLE_DESCRIPTOR, {count: quantity, days: rule.days})
				: i18n._(WAIVER_COUNTRY_DESCRIPTOR, {days: rule.days});
		}
		return quantity > 1 ? i18n._(WAIVER_MULTIPLE_DESCRIPTOR, {count: quantity}) : i18n._(WAIVER_DESCRIPTOR);
	};
	const renderDonation = (current: OperatorPassStateResponse) => {
		if (current.donation.presets_minor.length === 0) return null;
		if (!donationOpen) {
			return (
				<FocusRing offset={-2} data-flx="premium.operator-pass-checkout-modal.donation-add-focus-ring">
					<button
						type="button"
						className={styles.textButton}
						onClick={() => setDonationOpen(true)}
						disabled={submitting}
						data-flx="premium.operator-pass-checkout-modal.button.add-donation"
					>
						{i18n._(DONATION_ADD_DESCRIPTOR)}
					</button>
				</FocusRing>
			);
		}
		return (
			<div ref={donationRef} className={styles.donation} data-flx="premium.operator-pass-checkout-modal.donation">
				<h3 className={styles.sectionTitle} data-flx="premium.operator-pass-checkout-modal.donation-title">
					{i18n._(DONATION_LABEL_DESCRIPTOR)}
				</h3>
				<SegmentedTabs
					tabs={donationTabs}
					selectedTab={donationAmountMinor === null ? NO_DONATION_TAB_ID : String(donationAmountMinor)}
					onTabChange={(value) => {
						if (!submitting) setDonationAmountMinor(value === NO_DONATION_TAB_ID ? null : Number(value));
					}}
					ariaLabel={i18n._(DONATION_LABEL_DESCRIPTOR)}
					semantics="radio"
					data-flx="premium.operator-pass-checkout-modal.donation-options"
				/>
				{donationAmountMinor != null && (
					<p className={styles.total} data-flx="premium.operator-pass-checkout-modal.total">
						{i18n._(DUE_TODAY_DESCRIPTOR, {
							total: formatPrice(current.price_minor * quantity + donationAmountMinor, current.currency),
							donation: formatPrice(donationAmountMinor, current.donation.currency),
						})}
					</p>
				)}
			</div>
		);
	};
	const renderPurchase = (current: OperatorPassStateResponse) => {
		const holderCard = renderOperatorPassHolderCard({current, i18n});
		const priceBlock = renderOperatorPassPriceBlock({current, count: quantity, i18n, formatPrice});
		const summary = renderSummary();
		const quantityControl = renderQuantity(current);
		const listingUnchanged = !asksWallChoice ? (
			<p className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.listing-unchanged">
				{i18n._(OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR)}
			</p>
		) : null;
		const waiverLabel = (
			<span className={styles.consentLabel} data-flx="premium.operator-pass-checkout-modal.waiver-label">
				{waiverText(current)}
			</span>
		);
		const waiverError = waiverMissing && (
			<p
				id="operator-pass-checkout-waiver-error"
				className={styles.consentError}
				data-flx="premium.operator-pass-checkout-modal.waiver-error"
			>
				{i18n._(OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR)}
			</p>
		);
		const ageLabel = quantity > 1 && (
			<span className={styles.consentLabel} data-flx="premium.operator-pass-checkout-modal.age-label">
				{i18n._(AGE_CONFIRMATION_DESCRIPTOR)}
			</span>
		);
		const ageError = quantity > 1 && ageMissing && (
			<p
				id="operator-pass-checkout-age-error"
				className={styles.consentError}
				data-flx="premium.operator-pass-checkout-modal.age-error"
			>
				{i18n._(OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR)}
			</p>
		);
		return (
			<>
				{holderCard}
				{priceBlock}
				{summary}
				{quantityControl}
				<OperatorPassCheckoutForm
					draft={draft}
					values={{wallName, wallOptOut, wallNameProblem, waiver, ageConfirmed, waiverMissing, ageMissing}}
					asksWallChoice={asksWallChoice}
					quantity={quantity}
					submitting={submitting}
					listingUnchanged={listingUnchanged}
					waiverLabel={waiverLabel}
					waiverError={waiverError}
					ageLabel={ageLabel}
					ageError={ageError}
					data-flx="premium.operator-pass-checkout-modal.purchase-form"
				/>
				{renderDonation(current)}
				<PurchaseDisclaimer align="left" operatorPass data-flx="premium.operator-pass-checkout-modal.terms-line" />
			</>
		);
	};
	const showsPurchase = state != null && onSale && blockingNotice == null;
	const renderBody = () => {
		if (loadState.status === 'failed' && !state) {
			return (
				<Modal.Description data-flx="premium.operator-pass-checkout-modal.load-failed">
					{i18n._(LOAD_FAILED_DESCRIPTOR)}
				</Modal.Description>
			);
		}
		if (!state) {
			return (
				<div className={styles.loading} data-flx="premium.operator-pass-checkout-modal.loading">
					<Spinner data-flx="premium.operator-pass-checkout-modal.spinner" />
				</div>
			);
		}
		if (blockingNotice) return renderOperatorPassPriceBlock({current: state, count: 1, i18n, formatPrice});
		if (onSale) return renderPurchase(state);
		if (operator) return renderOwned(state);
		return renderWaitlist(state);
	};
	const renderFooterAction = () => {
		if (loadState.status === 'failed' && !state) {
			return (
				<Button
					onClick={() => void load()}
					variant="primary"
					data-flx="premium.operator-pass-checkout-modal.button.retry"
				>
					{i18n._(RETRY_DESCRIPTOR)}
				</Button>
			);
		}
		if (!state || blockingNotice) return null;
		if (!onSale && operator) {
			return (
				<Button onClick={openLabs} variant="primary" data-flx="premium.operator-pass-checkout-modal.button.labs">
					{i18n._(OPERATOR_PASS_OPEN_LABS_DESCRIPTOR, {labsName: PRODUCT_LABS_COMMUNITY_NAME})}
				</Button>
			);
		}
		if (!onSale) {
			return (
				<Button
					onClick={openWaitlist}
					variant="primary"
					data-flx="premium.operator-pass-checkout-modal.button.waitlist"
				>
					{i18n._(JOIN_WAITLIST_DESCRIPTOR)}
				</Button>
			);
		}
		return (
			<Button
				onClick={() => void handleContinue()}
				submitting={submitting}
				variant="primary"
				data-flx="premium.operator-pass-checkout-modal.button.continue"
			>
				{i18n._(CONTINUE_DESCRIPTOR)}
			</Button>
		);
	};
	const footerAction = renderFooterAction();
	return (
		<Modal.Root size="small" centered data-flx="premium.operator-pass-checkout-modal.modal-root">
			<Modal.Header title={OPERATOR_PASS_NAME} data-flx="premium.operator-pass-checkout-modal.modal-header" />
			<Modal.Content data-flx="premium.operator-pass-checkout-modal.modal-content">
				<Modal.ContentLayout data-flx="premium.operator-pass-checkout-modal.content-layout">
					{topNotice && (
						<div ref={noticeRef} data-flx="premium.operator-pass-checkout-modal.notice-anchor">
							{renderNotice(topNotice)}
						</div>
					)}
					{renderBody()}
				</Modal.ContentLayout>
			</Modal.Content>
			{footerAction ? (
				<Modal.Footer data-flx="premium.operator-pass-checkout-modal.modal-footer">
					<Button
						onClick={ModalCommands.pop}
						variant="secondary"
						data-flx="premium.operator-pass-checkout-modal.button.close"
					>
						{i18n._(showsPurchase ? CANCEL_DESCRIPTOR : CLOSE_DESCRIPTOR)}
					</Button>
					{footerAction}
				</Modal.Footer>
			) : (
				<Modal.DismissFooter data-flx="premium.operator-pass-checkout-modal.dismiss-footer" />
			)}
		</Modal.Root>
	);
});
