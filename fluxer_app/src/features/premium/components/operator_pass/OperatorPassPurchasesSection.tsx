// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import actionStyles from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection.module.css';
import {isOperatorPassAvailable} from '@app/features/premium/utils/OperatorPassUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {OperatorPassPurchaseResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

export const PURCHASE_TITLE_DESCRIPTOR = msg({
	message: 'Operator Pass purchase',
	comment: 'Title of a paid Operator Pass purchase in settings. Operator Pass is a product name.',
});
export const UNDELIVERED_LINE_DESCRIPTOR = msg({
	message: 'Paid {date}. Not delivered yet.',
	comment:
		'Line under a paid Operator Pass purchase that was not delivered yet. {date} is a date without a time, such as Nov 20, 2026.',
});
export const DOWNLOAD_CERTIFICATE_DESCRIPTOR = msg({
	message: 'Download certificate',
	comment:
		'Button next to an Operator Pass purchase that opens the Operator certificate, a PDF with a number and the name chosen at checkout, in the browser. Never translate certificate as diploma, card, license or token.',
});
export const DOWNLOAD_PAGE_DESCRIPTOR = msg({
	message: 'Download page {number} ({name})',
	comment:
		'Small text link after an Operator Pass purchase with more than one certificate page. It opens only that certificate page, as its own PDF, in the browser. {number} is the page number in the certificate PDF, starting at 1. {name} is the name chosen at checkout for that page, and several pages can have the same name. Never translate page as website or certificate as diploma, card, license or token.',
});
export const CERTIFICATE_PAGE_DESCRIPTOR = msg({
	message: 'Page {number} ({name})',
	comment:
		'Label of one Operator certificate page in the Operator Pass settings, for a purchase with more than one page or a pass the buyer offered or passed on to someone else. Download, Copy link and Reset link text links follow it. {number} is the page number in the certificate PDF, starting at 1. {name} is the name chosen at checkout for that page. Never translate page as website or certificate as diploma, card, license or token.',
});
export const RESET_LINK_DESCRIPTOR = msg({
	message: 'Reset link',
	comment:
		'Small text link next to an Operator certificate page in the Operator Pass settings. It replaces the private link to that certificate page with a new one, after a confirmation.',
});
const RESET_LINK_TITLE_DESCRIPTOR = msg({
	message: 'Reset this certificate link?',
	comment:
		'Title of the confirmation dialog before replacing the private link to an Operator certificate page with a new one. Never translate certificate as diploma, card, license or token.',
});
const RESET_LINK_BODY_DESCRIPTOR = msg({
	message:
		"The current link stops working right away, for you and for anyone you've shared it with. We make a new copy of this page with the new link printed on it, with the same number and date.",
	comment:
		'Body of the confirmation dialog before replacing the private link to an Operator certificate page. The certificate is a PDF, and each page has a number and the date and time of payment. The new copy of the page has the new link printed on it.',
});
const RESET_LINK_DONE_DESCRIPTOR = msg({
	message: 'Your certificate page has a new link.',
	comment: 'Toast after replacing the private link to an Operator certificate page with a new one.',
});
const RESET_LINK_FAILED_DESCRIPTOR = msg({
	message: "Couldn't reset the link. Try again.",
	comment: 'Error toast when replacing the private link to an Operator certificate page failed.',
});
const LOAD_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load your Operator Pass purchases.",
	comment:
		'Muted line in settings when the list of paid Operator Pass purchases could not be loaded. A Try again button follows. Operator Pass is a product name.',
});

const logger = new Logger('OperatorPassPurchases');

export function openCertificateLinkResetModal(i18n: I18n, passId: string, onReset: () => void): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(RESET_LINK_TITLE_DESCRIPTOR)}
				description={i18n._(RESET_LINK_BODY_DESCRIPTOR)}
				primaryText={i18n._(RESET_LINK_DESCRIPTOR)}
				primaryVariant="danger"
				onPrimary={async () => {
					try {
						await OperatorPassCommands.resetCertificateLink(passId);
						ToastCommands.success(i18n._(RESET_LINK_DONE_DESCRIPTOR));
						onReset();
					} catch (error) {
						logger.warn('Failed to reset an Operator certificate link', error);
						ToastCommands.error(i18n._(RESET_LINK_FAILED_DESCRIPTOR));
					}
				}}
				data-flx="premium.operator-pass-purchases-section.reset-link-confirm-modal"
			/>
		)),
	);
}

export function useOperatorPassPurchases(): {
	purchases: ReadonlyArray<OperatorPassPurchaseResponse>;
	failed: boolean;
	reload: () => void;
} {
	const [purchases, setPurchases] = useState<ReadonlyArray<OperatorPassPurchaseResponse>>([]);
	const [failed, setFailed] = useState(false);
	const reload = useCallback(() => {
		OperatorPassCommands.fetchPurchases()
			.then((result) => {
				setPurchases(result.filter((purchase) => purchase.can_withdraw || purchase.certificate_available));
				setFailed(false);
			})
			.catch((error) => {
				logger.warn('Failed to load Operator Pass purchases', error);
				setFailed(true);
			});
	}, []);
	useEffect(() => {
		if (isOperatorPassAvailable()) reload();
	}, [reload]);
	return {purchases, failed, reload};
}

export const OperatorPassPurchasesLoadFailed = observer(function OperatorPassPurchasesLoadFailed({
	onRetry,
	className,
}: {
	onRetry: () => void;
	className?: string;
}) {
	const {i18n} = useLingui();
	return (
		<div
			className={clsx(actionStyles.loadFailed, className)}
			data-flx="premium.operator-pass-purchases-section.load-failed"
		>
			<p className={actionStyles.loadFailedText} data-flx="premium.operator-pass-purchases-section.load-failed-text">
				{i18n._(LOAD_FAILED_DESCRIPTOR)}
			</p>
			<FocusRing offset={-2} data-flx="premium.operator-pass-purchases-section.button.retry.focus-ring">
				<button
					type="button"
					className={actionStyles.pageLink}
					onClick={onRetry}
					data-flx="premium.operator-pass-purchases-section.button.retry"
				>
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</button>
			</FocusRing>
		</div>
	);
});
