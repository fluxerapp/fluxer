// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {OPERATOR_PASS_NAME, PAYMENT_PROVIDER_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import {businessPurchaseCnpj, CNPJ_INVALID_DESCRIPTOR} from '@app/features/premium/components/BusinessPurchaseField';
import styles from '@app/features/premium/components/modals/OperatorPassCheckoutModal.module.css';
import {CheckoutAgreeStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutAgreeStage';
import {CheckoutChooseStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutChooseStage';
import {CheckoutDetailsStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutDetailsStage';
import {CheckoutDoneStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutDoneStage';
import {CheckoutNoticeStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutNoticeStage';
import {CheckoutOutcomeStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutOutcomeStage';
import stageStyles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {CheckoutWaitingStage} from '@app/features/premium/components/operator_pass/checkout/CheckoutWaitingStage';
import {
	AGE_BOX_ID,
	CERTIFICATE_NAME_FIELD_ID,
	DELIVERY_BOX_ID,
	OperatorPassCheckoutDraft,
	WALL_NAME_FIELD_ID,
} from '@app/features/premium/components/operator_pass/checkout/OperatorPassCheckoutDraft';
import {OperatorPassShowcase} from '@app/features/premium/components/operator_pass/checkout/OperatorPassShowcase';
import {OpenOperatorsButton} from '@app/features/premium/components/operator_pass/OpenOperatorsButton';
import {asksOperatorPassLabsGreeting} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import {DOWNLOAD_CERTIFICATE_DESCRIPTOR} from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection';
import OperatorPassCheckoutSession from '@app/features/premium/state/OperatorPassCheckoutSession';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	type CheckoutEvent,
	type CheckoutStage,
	checkoutEventForPurchases,
	nextCheckoutStage,
	stageForCheckoutError,
} from '@app/features/premium/utils/OperatorPassCheckoutMachine';
import {
	clampOperatorPassQuantity,
	focusFirstInvalidOperatorPassField,
	focusOperatorPassField,
	getOperatorPassErrorCode,
	getOperatorPassWallNameProblem,
	normalizeOperatorPassWallName,
} from '@app/features/premium/utils/OperatorPassUtils';
import {getPremiumPurchaseBlockedReason} from '@app/features/premium/utils/PremiumPurchaseBlock';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Confetti} from '@app/features/ui/confetti/Confetti';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import Users from '@app/features/user/state/Users';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import WindowState from '@app/features/window/state/Window';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import {
	OPERATOR_PASS_CURRENCIES,
	OPERATOR_PASS_PHASES,
	type OperatorPassCurrency,
	type OperatorPassPhase,
	type OperatorPassPriceChangedErrorData,
	type OperatorPassPurchaseResponse,
	type OperatorPassStateResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useEffect, useRef, useState} from 'react';

export const OPERATOR_PASS_CHECKOUT_MODAL_KEY = 'operator-pass-checkout';

type InlineNotice =
	| {kind: 'canceled'}
	| {kind: 'price_changed'; price: string}
	| {kind: 'rate_limited'}
	| {kind: 'failed'};

type CarouselStep = 'choose' | 'details' | 'agree' | 'waiting' | 'end';

const CAROUSEL_STEPS: ReadonlyArray<CarouselStep> = ['choose', 'details', 'agree', 'waiting', 'end'];

const FAST_POLL_MS = 2000;
const SLOW_POLL_MS = 5000;
const FAST_POLL_WINDOW_MS = 30_000;

const CHOOSE_TITLE_DESCRIPTOR = msg({
	message: 'Get an {operatorPassName}',
	comment:
		'Title of the first step of the Operator Pass checkout dialog, where the buyer picks how many passes and the names on the certificate. Operator Pass is a product name and stays untranslated.',
});
const DETAILS_TITLE_DESCRIPTOR = msg({
	message: 'Your details',
	comment:
		"Title of the second step of the Operator Pass checkout dialog, with the name on the Operators' wall, the welcome in #operators and whether the buyer buys for a company.",
});
const AGREE_TITLE_DESCRIPTOR = msg({
	message: 'Check and pay',
	comment:
		'Title of the third step of the Operator Pass checkout dialog, with the total price and the required checkboxes before the payment page opens.',
});
const WAITING_TITLE_DESCRIPTOR = msg({
	message: 'Finish paying',
	comment: 'Title of the Operator Pass checkout dialog while the app waits for the payment on the payment page.',
});
const DONE_TITLE_DESCRIPTOR = msg({
	message: 'Thank you',
	comment: 'Title of the Operator Pass checkout dialog after the payment went through and the pass is on the account.',
});
const REFUNDED_TITLE_DESCRIPTOR = msg({
	message: 'Payment refunded',
	comment:
		'Title of the Operator Pass checkout dialog after a payment that we refunded in full right away, with no certificate created.',
});
const EXPIRED_TITLE_DESCRIPTOR = msg({
	message: 'Payment page expired',
	comment: 'Title of the Operator Pass checkout dialog when the payment page timed out before anyone paid.',
});
const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue',
	comment: 'Primary button in the Operator Pass checkout dialog that goes to the next step.',
});
const BACK_DESCRIPTOR = msg({
	message: 'Back',
	comment: 'Button in the Operator Pass checkout dialog that goes back to the previous step.',
});
const PAY_DESCRIPTOR = msg({
	message: 'Continue to payment',
	comment: 'Primary button in the Operator Pass checkout dialog. Opens the payment page.',
});
const START_AGAIN_DESCRIPTOR = msg({
	message: 'Start again',
	comment:
		'Primary button in the Operator Pass checkout dialog after the payment page expired. It goes back to the step with the checkboxes to open a new payment page.',
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

const logger = new Logger('OperatorPassCheckoutModal');

function isPhase(value: unknown): value is OperatorPassPhase {
	return OPERATOR_PASS_PHASES.some((phase) => phase === value);
}

function isCurrency(value: unknown): value is OperatorPassCurrency {
	return OPERATOR_PASS_CURRENCIES.some((currency) => currency === value);
}

function certificateNameErrorIndex(error: unknown, fieldCount: number): number {
	if (!(error instanceof HttpError)) return 0;
	const body = error.body as Record<string, unknown> | null | undefined;
	const index = body?.index;
	return typeof index === 'number' && Number.isInteger(index) && index >= 0 && index < fieldCount ? index : 0;
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

function carouselStep(stage: CheckoutStage): CarouselStep | null {
	switch (stage.kind) {
		case 'choose':
		case 'details':
		case 'agree':
		case 'waiting':
			return stage.kind;
		case 'done':
		case 'outcome':
			return 'end';
		default:
			return null;
	}
}

function certificateFieldId(index: number): string {
	return index > 0 ? `${CERTIFICATE_NAME_FIELD_ID}-${index + 1}` : CERTIFICATE_NAME_FIELD_ID;
}

async function readRunPurchases(sessionIds: ReadonlyArray<string>): Promise<Array<OperatorPassPurchaseResponse>> {
	let purchases: Array<OperatorPassPurchaseResponse> = [];
	for (const sessionId of sessionIds) {
		purchases = await OperatorPassCommands.fetchPurchases(sessionId);
	}
	return purchases;
}

interface OperatorPassCheckoutModalProps {
	canceled: boolean;
}

export const OperatorPassCheckoutModal = observer(function OperatorPassCheckoutModal({
	canceled,
}: OperatorPassCheckoutModalProps) {
	const {i18n} = useLingui();
	const mobile = MobileLayout.enabled;
	const [stage, setStage] = useState<CheckoutStage>(() =>
		OperatorPassCheckoutSession.waiting ? {kind: 'waiting'} : {kind: 'loading'},
	);
	const [state, setState] = useState<OperatorPassStateResponse | null>(null);
	const [draft] = useState(() => new OperatorPassCheckoutDraft(Users.currentUser?.displayName ?? ''));
	const [submitting, setSubmitting] = useState(false);
	const [notice, setNotice] = useState<InlineNotice | null>(canceled ? {kind: 'canceled'} : null);
	const pendingFocusRef = useRef<string | null>(null);
	const generationRef = useRef(0);
	const mountedRef = useRef(true);
	const pollNowRef = useRef<() => void>(() => {});
	const dispatch = useCallback((event: CheckoutEvent) => {
		setStage((current) => nextCheckoutStage(current, event));
	}, []);
	useEffect(() => {
		mountedRef.current = true;
		return () => {
			mountedRef.current = false;
		};
	}, []);
	const load = useCallback(async () => {
		const generation = ++generationRef.current;
		try {
			const next = await OperatorPassCommands.fetchState();
			if (!mountedRef.current || generation !== generationRef.current) return;
			setState(next);
			draft.setQuantity(clampOperatorPassQuantity(draft.quantity, next.max_quantity));
			dispatch({type: 'loaded', state: next});
		} catch (error) {
			logger.error('Failed to load Operator Pass state', error);
			if (!mountedRef.current || generation !== generationRef.current) return;
			dispatch({type: 'load_failed'});
		}
	}, [dispatch, draft]);
	useEffect(() => {
		void load();
		if (OperatorPassHoldings.passes === null) {
			OperatorPassCommands.fetchPasses().catch((error) => logger.warn('Failed to load Operator Passes', error));
		}
	}, [load]);
	const accountKey = [
		Users.currentUser?.accountTier ?? '',
		Users.currentUser?.email ?? '',
		Users.currentUser?.emailBounced ? 'bounced' : '',
	].join(':');
	const accountKeyRef = useRef(accountKey);
	useEffect(() => {
		if (accountKeyRef.current === accountKey) return;
		accountKeyRef.current = accountKey;
		void load();
	}, [accountKey, load]);
	const locale = getCurrentLocale();
	const formatPrice = useCallback(
		(amountMinor: number, currency: string) =>
			formatMinorUnitPrice(amountMinor, currency, locale, OperatorPassCommands.getCountryCode()),
		[locale],
	);
	const asksWallChoice = !state?.holder?.wall_chosen || draft.revealWallField;
	const asksLabsGreeting = asksOperatorPassLabsGreeting();
	const asksCnpj = state?.withdrawal_rule?.country === 'BR';
	const businessCnpj = businessPurchaseCnpj(draft.isBusiness, asksCnpj, draft.cnpj);
	const businessBox = typeof businessCnpj === 'string';
	const businessBoxRef = useRef(businessBox);
	useEffect(() => {
		if (businessBoxRef.current === businessBox) return;
		businessBoxRef.current = businessBox;
		draft.resetDelivery();
	}, [businessBox, draft]);
	const boxTermsKey = state
		? `${state.currency}:${state.withdrawal_rule?.country ?? ''}:${state.withdrawal_rule?.days ?? ''}`
		: null;
	const boxTermsKeyRef = useRef(boxTermsKey);
	useEffect(() => {
		if (boxTermsKeyRef.current === boxTermsKey) return;
		boxTermsKeyRef.current = boxTermsKey;
		draft.resetConsents();
	}, [boxTermsKey, draft]);
	const settled = stage.kind === 'done' || stage.kind === 'outcome';
	useEffect(() => {
		if (settled) OperatorPassCheckoutSession.clear();
	}, [settled]);
	const polling = stage.kind === 'waiting' || (stage.kind === 'done' && !stage.purchase.certificate_available);
	useEffect(() => {
		if (!polling) return;
		const run = OperatorPassCheckoutSession.run;
		const sessionIds = run?.sessionIds ?? (stage.kind === 'done' ? [stage.purchase.id] : []);
		const expiresAt = run?.expiresAt ?? 0;
		if (sessionIds.length === 0) return;
		let active = true;
		let inFlight = false;
		let again = false;
		let timer: ReturnType<typeof setTimeout> | null = null;
		const startedAt = Date.now();
		const schedule = () => {
			if (!active || !WindowState.visible) return;
			const delay = Date.now() - startedAt < FAST_POLL_WINDOW_MS ? FAST_POLL_MS : SLOW_POLL_MS;
			timer = setTimeout(() => void poll(), delay);
		};
		const poll = async () => {
			if (timer) clearTimeout(timer);
			timer = null;
			if (inFlight) {
				again = true;
				return;
			}
			inFlight = true;
			try {
				const purchases = await readRunPurchases(sessionIds);
				if (!active) return;
				const event = checkoutEventForPurchases(purchases, sessionIds, expiresAt, Date.now());
				if (event) dispatch(event);
				if (event?.type === 'refunded' || event?.type === 'expired') return;
				if (event?.type === 'completed' && event.purchase.certificate_available) return;
			} catch (error) {
				logger.warn('Failed to check the Operator Pass payment', error);
			} finally {
				inFlight = false;
			}
			if (!active) return;
			if (again) {
				again = false;
				void poll();
				return;
			}
			schedule();
		};
		pollNowRef.current = () => void poll();
		void poll();
		return () => {
			active = false;
			if (timer) clearTimeout(timer);
			pollNowRef.current = () => {};
		};
	}, [polling, dispatch]);
	const passCount = OperatorPassHoldings.count;
	const operatorSince = Users.currentUser?.operatorPassSince?.getTime() ?? null;
	const windowVisible = WindowState.visible;
	const wakeKey = `${passCount}:${operatorSince}:${windowVisible}:${WindowState.focused}`;
	const wakeKeyRef = useRef(wakeKey);
	useEffect(() => {
		if (wakeKeyRef.current === wakeKey) return;
		wakeKeyRef.current = wakeKey;
		if (windowVisible) pollNowRef.current();
	}, [wakeKey, windowVisible]);
	const focusAfterStep = (id: string) => {
		pendingFocusRef.current = id;
	};
	const handleStepShown = () => {
		const id = pendingFocusRef.current;
		if (!id) return;
		pendingFocusRef.current = null;
		focusOperatorPassField(id);
	};
	const openPaymentPage = (url: string) => {
		if (!mobile) {
			void openExternalUrl(url);
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
					onPrimary={() => void openExternalUrl(url)}
					data-flx="premium.operator-pass-checkout-modal.open-checkout-url.confirm-modal"
				/>
			)),
		);
	};
	const handleCheckoutError = (error: unknown, current: OperatorPassStateResponse) => {
		logger.error('Operator Pass checkout failed', error);
		const code = getOperatorPassErrorCode(error);
		if (code === APIErrorCodes.OPERATOR_PASS_PRICE_CHANGED) {
			const data = priceChangedData(error);
			if (!data || data.currency !== current.currency) {
				void load();
				setNotice(
					data ? {kind: 'price_changed', price: formatPrice(data.price_minor, data.currency)} : {kind: 'failed'},
				);
				return;
			}
			setState({...current, ...data});
			setNotice({kind: 'price_changed', price: formatPrice(data.price_minor, data.currency)});
			return;
		}
		const target = stageForCheckoutError(
			code,
			error instanceof HttpError ? getPremiumPurchaseBlockedReason(error.body) : null,
		);
		if (target === null) {
			setNotice(
				error instanceof HttpError && error.status === HttpStatus.TOO_MANY_REQUESTS
					? {kind: 'rate_limited'}
					: {kind: 'failed'},
			);
			return;
		}
		if (target === 'choose') {
			const index = certificateNameErrorIndex(error, draft.certificateNamesToSend.length);
			draft.setCertificateNameProblem('rejected', index);
			focusAfterStep(certificateFieldId(index));
		} else if (target === 'details') {
			draft.setRevealWallField(true);
			draft.setWallNameProblem(code === APIErrorCodes.OPERATOR_PASS_WALL_NAME_REJECTED ? 'rejected' : 'required');
			focusAfterStep(WALL_NAME_FIELD_ID);
		} else if (target === 'agree') {
			if (code === APIErrorCodes.OPERATOR_PASS_AGE_CONFIRMATION_REQUIRED) {
				draft.setAgeConfirmed(false);
				draft.setAgeMissing(true);
				focusOperatorPassField(AGE_BOX_ID);
			} else {
				draft.setDelivery(false);
				draft.setDeliveryMissing(true);
				focusOperatorPassField(DELIVERY_BOX_ID);
			}
			return;
		}
		dispatch({type: 'checkout_refused', stage: target});
	};
	const handleChooseContinue = () => {
		const problem = draft.findCertificateNameProblem();
		draft.setCertificateNameProblem(problem?.problem ?? null, problem?.index ?? null);
		if (problem) {
			focusOperatorPassField(certificateFieldId(problem.index));
			return;
		}
		setNotice(null);
		dispatch({type: 'continue'});
	};
	const handleDetailsContinue = () => {
		if (businessCnpj === null) {
			draft.setCnpjError(i18n._(CNPJ_INVALID_DESCRIPTOR));
			return;
		}
		const problem = asksWallChoice ? getOperatorPassWallNameProblem(draft.wallName, draft.wallOptOut) : null;
		draft.setWallNameProblem(problem);
		if (problem) {
			focusOperatorPassField(WALL_NAME_FIELD_ID);
			return;
		}
		dispatch({type: 'continue'});
	};
	const handlePay = async () => {
		if (!state || submitting) return;
		const current = state;
		draft.setDeliveryMissing(!draft.delivery);
		draft.setAgeMissing(!draft.ageConfirmed);
		if (
			focusFirstInvalidOperatorPassField([
				{id: DELIVERY_BOX_ID, invalid: !draft.delivery},
				{id: AGE_BOX_ID, invalid: !draft.ageConfirmed},
			])
		) {
			return;
		}
		setSubmitting(true);
		setNotice(null);
		const startedPassCount = OperatorPassHoldings.count ?? current.holder?.count ?? 0;
		try {
			const created = await OperatorPassCommands.createCheckout({
				quantity: draft.quantity,
				country_code: OperatorPassCommands.getCountryCode(),
				quote: current.quote,
				delivery_consent: true,
				certificate_names: draft.certificateNamesToSend,
				consent_locale: locale,
				age_confirmed: true,
				is_business: draft.isBusiness,
				...(businessCnpj ? {cnpj: businessCnpj} : {}),
				...(asksWallChoice
					? {
							wall_name: draft.wallOptOut ? null : normalizeOperatorPassWallName(draft.wallName),
							wall_opt_out: draft.wallOptOut,
						}
					: {}),
				...(asksLabsGreeting ? {labs_greeting: draft.labsGreeting} : {}),
			});
			if (!mountedRef.current) return;
			OperatorPassCheckoutSession.addSession(
				{id: created.session_id, url: created.url, expiresAt: Date.parse(created.expires_at)},
				startedPassCount,
			);
			dispatch({type: 'checkout_started'});
			openPaymentPage(created.url);
		} catch (error) {
			if (!mountedRef.current) return;
			handleCheckoutError(error, current);
		} finally {
			if (mountedRef.current) setSubmitting(false);
		}
	};
	const handleStartOver = () => {
		draft.resetConsents();
		setNotice(null);
		dispatch({type: 'start_over'});
		void load();
	};
	const openOperatorPassSettings = () => {
		ModalCommands.popWithKey(OPERATOR_PASS_CHECKOUT_MODAL_KEY);
		ModalCommands.push(
			modal(
				() => (
					<UserSettingsModal
						initialTab="operator_pass"
						data-flx="premium.operator-pass-checkout-modal.user-settings-modal"
					/>
				),
				'user-settings',
			),
		);
	};
	const closeModal = () => ModalCommands.popWithKey(OPERATOR_PASS_CHECKOUT_MODAL_KEY);
	const renderNotice = (): ReactNode => {
		if (!notice) return null;
		if (notice.kind === 'canceled') {
			return (
				<p role="status" className={stageStyles.note} data-flx="premium.operator-pass-checkout-modal.notice">
					{i18n._(CANCELED_NOTICE_DESCRIPTOR)}
				</p>
			);
		}
		let text = i18n._(FAILED_NOTICE_DESCRIPTOR);
		if (notice.kind === 'price_changed') text = i18n._(PRICE_CHANGED_NOTICE_DESCRIPTOR, {price: notice.price});
		else if (notice.kind === 'rate_limited') text = i18n._(RATE_LIMITED_NOTICE_DESCRIPTOR);
		return (
			<div role="alert" data-flx="premium.operator-pass-checkout-modal.notice-alert">
				<WarningAlert data-flx="premium.operator-pass-checkout-modal.notice">{text}</WarningAlert>
			</div>
		);
	};
	const title = (() => {
		switch (stage.kind) {
			case 'choose':
				return i18n._(CHOOSE_TITLE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME});
			case 'details':
				return i18n._(DETAILS_TITLE_DESCRIPTOR);
			case 'agree':
				return i18n._(AGREE_TITLE_DESCRIPTOR);
			case 'waiting':
				return i18n._(WAITING_TITLE_DESCRIPTOR);
			case 'done':
				return i18n._(DONE_TITLE_DESCRIPTOR);
			case 'outcome':
				return i18n._(stage.result === 'refunded' ? REFUNDED_TITLE_DESCRIPTOR : EXPIRED_TITLE_DESCRIPTOR);
			default:
				return OPERATOR_PASS_NAME;
		}
	})();
	const renderStepBody = (): ReactNode => {
		switch (stage.kind) {
			case 'choose':
				return state ? (
					<CheckoutChooseStage
						draft={draft}
						maxQuantity={state.max_quantity}
						notice={renderNotice()}
						data-flx="premium.operator-pass-checkout-modal.render-step-body.checkout-choose-stage"
					/>
				) : null;
			case 'details':
				return (
					<CheckoutDetailsStage
						draft={draft}
						asksWallChoice={asksWallChoice}
						asksLabsGreeting={asksLabsGreeting}
						asksCnpj={asksCnpj}
						data-flx="premium.operator-pass-checkout-modal.render-step-body.checkout-details-stage"
					/>
				);
			case 'agree':
				return state ? (
					<CheckoutAgreeStage
						draft={draft}
						state={state}
						businessBox={businessBox}
						submitting={submitting}
						notice={renderNotice()}
						formatPrice={formatPrice}
						data-flx="premium.operator-pass-checkout-modal.render-step-body.checkout-agree-stage"
					/>
				) : null;
			case 'waiting': {
				const url = OperatorPassCheckoutSession.run?.url ?? '';
				return (
					<CheckoutWaitingStage
						canReopen={url !== ''}
						onReopen={() => void openExternalUrl(url)}
						onStartOver={handleStartOver}
						data-flx="premium.operator-pass-checkout-modal.render-step-body.checkout-waiting-stage"
					/>
				);
			}
			case 'done':
				return (
					<CheckoutDoneStage
						purchase={stage.purchase}
						onOpenSettings={openOperatorPassSettings}
						data-flx="premium.operator-pass-checkout-modal.render-step-body.checkout-done-stage"
					/>
				);
			case 'outcome':
				return (
					<CheckoutOutcomeStage
						result={stage.result}
						purchase={stage.purchase}
						data-flx="premium.operator-pass-checkout-modal.render-step-body.checkout-outcome-stage"
					/>
				);
			default:
				return null;
		}
	};
	const renderBody = (): ReactNode => {
		const step = carouselStep(stage);
		if (step) {
			return (
				<SteppedCarousel
					step={step}
					steps={CAROUSEL_STEPS}
					focusOnStepChange
					onStepShown={handleStepShown}
					data-flx="premium.operator-pass-checkout-modal.carousel"
				>
					{renderStepBody()}
				</SteppedCarousel>
			);
		}
		switch (stage.kind) {
			case 'loading':
				return (
					<div
						className={clsx(stageStyles.stage, stageStyles.centered)}
						data-flx="premium.operator-pass-checkout-modal.loading"
					>
						<Spinner data-flx="premium.operator-pass-checkout-modal.spinner" />
					</div>
				);
			case 'failed':
				return (
					<p className={stageStyles.lead} data-flx="premium.operator-pass-checkout-modal.load-failed">
						{i18n._(LOAD_FAILED_DESCRIPTOR)}
					</p>
				);
			case 'notice':
				return (
					<CheckoutNoticeStage
						notice={stage.notice}
						state={state}
						formatPrice={formatPrice}
						data-flx="premium.operator-pass-checkout-modal.render-body.checkout-notice-stage"
					/>
				);
			default:
				return null;
		}
	};
	const backButton = (
		<Button
			variant="secondary"
			className={styles.back}
			onClick={() => dispatch({type: 'back'})}
			disabled={submitting}
			data-flx="premium.operator-pass-checkout-modal.button.back"
		>
			{i18n._(BACK_DESCRIPTOR)}
		</Button>
	);
	const renderFooterActions = (): ReactNode => {
		switch (stage.kind) {
			case 'failed':
				return (
					<Button
						variant="primary"
						onClick={() => {
							dispatch({type: 'retry'});
							void load();
						}}
						data-flx="premium.operator-pass-checkout-modal.button.retry"
					>
						{i18n._(RETRY_DESCRIPTOR)}
					</Button>
				);
			case 'choose':
				return (
					<Button
						variant="primary"
						onClick={handleChooseContinue}
						data-flx="premium.operator-pass-checkout-modal.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				);
			case 'details':
				return (
					<>
						{backButton}
						<Button
							variant="primary"
							onClick={handleDetailsContinue}
							data-flx="premium.operator-pass-checkout-modal.button.continue"
						>
							{i18n._(CONTINUE_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'agree':
				return (
					<>
						{backButton}
						<Button
							variant="primary"
							onClick={() => void handlePay()}
							submitting={submitting}
							data-flx="premium.operator-pass-checkout-modal.button.pay"
						>
							{i18n._(PAY_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'done': {
				const pages = stage.purchase.pages;
				const certificateUrl = pages.length === 1 ? pages[0].certificate_url : null;
				return (
					<>
						{certificateUrl && (
							<Button
								variant="secondary"
								onClick={() => void openExternalUrl(certificateUrl)}
								data-flx="premium.operator-pass-checkout-modal.button.download-certificate"
							>
								{i18n._(DOWNLOAD_CERTIFICATE_DESCRIPTOR)}
							</Button>
						)}
						<OpenOperatorsButton
							variant="primary"
							onOpened={closeModal}
							data-flx="premium.operator-pass-checkout-modal.button.operators"
						/>
					</>
				);
			}
			case 'outcome':
				return stage.result === 'unavailable' ? (
					<Button
						variant="primary"
						onClick={handleStartOver}
						data-flx="premium.operator-pass-checkout-modal.button.start-again"
					>
						{i18n._(START_AGAIN_DESCRIPTOR)}
					</Button>
				) : null;
			default:
				return null;
		}
	};
	const footerActions = renderFooterActions();
	const showcase =
		!mobile || stage.kind === 'choose' ? (
			<OperatorPassShowcase
				resting={stage.kind === 'waiting' || stage.kind === 'done'}
				compact={mobile}
				data-flx="premium.operator-pass-checkout-modal.showcase"
			/>
		) : null;
	return (
		<Modal.Root size="large" centered data-flx="premium.operator-pass-checkout-modal.modal-root">
			<div
				className={clsx(styles.layout, !mobile && styles.split)}
				data-flx="premium.operator-pass-checkout-modal.layout"
			>
				{!mobile && showcase}
				<div className={styles.main} data-flx="premium.operator-pass-checkout-modal.main">
					{stage.kind === 'done' && <Confetti data-flx="premium.operator-pass-checkout-modal.confetti" />}
					<div className={styles.header} data-flx="premium.operator-pass-checkout-modal.header">
						<Modal.Header title={title} data-flx="premium.operator-pass-checkout-modal.modal-header" />
					</div>
					{mobile && showcase}
					<Modal.Content className={styles.content} data-flx="premium.operator-pass-checkout-modal.modal-content">
						{renderBody()}
					</Modal.Content>
					{footerActions ? (
						<Modal.Footer className={styles.footer} data-flx="premium.operator-pass-checkout-modal.modal-footer">
							{footerActions}
						</Modal.Footer>
					) : (
						<Modal.DismissFooter data-flx="premium.operator-pass-checkout-modal.dismiss-footer" />
					)}
				</div>
			</div>
		</Modal.Root>
	);
});
