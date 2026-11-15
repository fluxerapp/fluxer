// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {DOWNLOAD_PAGE_DESCRIPTOR} from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type {OperatorPassPurchaseResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';

const DONE_EMAILED_DESCRIPTOR = msg({
	message:
		'Your {operatorPassName} is ready. We emailed your certificate, and you can download it here or in your settings.',
	comment:
		'Text after a successful Operator Pass purchase, when the certificate email went out. The certificate is the Operator certificate PDF with a number and the name chosen at checkout. Settings means the user settings. Operator Pass is a product name and stays untranslated.',
});
const DONE_READY_DESCRIPTOR = msg({
	message: 'Your {operatorPassName} is ready, and you can download your certificate here or in your settings.',
	comment:
		"Text after a successful Operator Pass purchase, when the certificate can be downloaded but the email with it hasn't gone out yet. The certificate is the Operator certificate PDF. Settings means the user settings. Don't say the email was sent. Operator Pass is a product name and stays untranslated.",
});
const DONE_PENDING_DESCRIPTOR = msg({
	message:
		"Your {operatorPassName} is ready. We're still making your certificate, and it shows up in your settings once it's done.",
	comment:
		"Text after a successful Operator Pass purchase, when the certificate PDF can't be downloaded yet. Settings means the user settings. Don't promise when it is done or say it was emailed. Operator Pass is a product name and stays untranslated.",
});

interface CheckoutDoneStageProps {
	purchase: OperatorPassPurchaseResponse;
	onOpenSettings: () => void;
}

export function CheckoutDoneStage({purchase, onOpenSettings}: CheckoutDoneStageProps) {
	const {i18n} = useLingui();
	let body = DONE_PENDING_DESCRIPTOR;
	if (purchase.certificate_emailed) body = DONE_EMAILED_DESCRIPTOR;
	else if (purchase.certificate_available) body = DONE_READY_DESCRIPTOR;
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.done">
			<p className={styles.lead} aria-live="polite" data-flx="premium.operator-pass-checkout.done-body">
				{i18n._(body, {operatorPassName: OPERATOR_PASS_NAME})}
			</p>
			{purchase.pages.length > 1 &&
				purchase.pages.map((page, index) => {
					const url = page.certificate_url;
					if (!url) return null;
					return (
						<p key={page.pass_id} className={styles.note} data-flx="premium.operator-pass-checkout.done-page">
							<FocusRing offset={-2} data-flx="premium.operator-pass-checkout.page-link-focus-ring">
								<button
									type="button"
									className={styles.inlineLink}
									onClick={() => void openExternalUrl(url)}
									data-flx="premium.operator-pass-checkout.button.download-page"
								>
									{i18n._(DOWNLOAD_PAGE_DESCRIPTOR, {number: index + 1, name: page.name})}
								</button>
							</FocusRing>
						</p>
					);
				})}
			<p className={styles.note} data-flx="premium.operator-pass-checkout.done-settings">
				<Trans comment="Line after a successful Operator Pass purchase. The linked text opens the Operator Pass tab in the user settings, where the person can change how they show on the Operators' wall and the welcome in #operators. The Operators' wall is the list of Operators on the homepage. Keep #operators as is, it is a channel name. {OPERATOR_PASS_NAME} is a product name and stays untranslated.">
					You can change your spot on the Operators' wall and your welcome in #operators in{' '}
					<FocusRing offset={-2} data-flx="premium.operator-pass-checkout.settings-link-focus-ring">
						<button
							type="button"
							className={styles.inlineLink}
							onClick={onOpenSettings}
							data-flx="premium.operator-pass-checkout.button.settings"
						>
							your {OPERATOR_PASS_NAME} settings
						</button>
					</FocusRing>
					.
				</Trans>
			</p>
		</div>
	);
}
