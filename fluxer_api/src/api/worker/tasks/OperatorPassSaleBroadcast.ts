// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {OperatorPassWaitlistRow} from '@app/api/database/types/OperatorPassTypes';
import {OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR} from '@app/api/operator_pass/OperatorPassPrices';
import {
	getOperatorPassPhase,
	operatorPassNow,
	resolveOperatorPassCurrency,
} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	type OperatorPassWaitlistBroadcast,
	type OperatorPassWaitlistBroadcastDependencies,
	runOperatorPassWaitlistBroadcast,
} from '@app/api/operator_pass/OperatorPassWaitlistBroadcast';
import {buildOperatorPassUnsubscribeUrls} from '@app/api/operator_pass/OperatorPassWaitlistTokens';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import type {IBulkEmailSession} from '@pkgs/email/src/IEmailService';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const OPERATOR_PASS_SALE_BROADCAST_LOCK_KEY = 'operator_pass:sale_broadcast:lock';
const OPERATOR_PASS_SALE_EMAIL_CLAIM_PREFIX = 'operator_pass:sale_email:claim:';

function formatOperatorPassEarlyBirdPrice(entry: OperatorPassWaitlistRow): string {
	const currency = resolveOperatorPassCurrency(entry.country_code);
	return formatMinorUnitPrice(
		OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR[currency],
		currency,
		entry.locale,
		entry.country_code,
	);
}

async function sendSaleEmail(sender: IBulkEmailSession, entry: OperatorPassWaitlistRow): Promise<boolean> {
	const {priceRisesAt} = Config.operatorPass;
	if (!priceRisesAt) {
		return false;
	}
	const {unsubscribeUrl, unsubscribePageUrl} = buildOperatorPassUnsubscribeUrls(entry.entry_id);
	return sender.send(
		'operator_pass_sale_open',
		entry.email,
		{
			checkoutUrl: `${Config.endpoints.webApp}/channels/@operator-pass`,
			priceFormatted: formatOperatorPassEarlyBirdPrice(entry),
			priceRisesAt,
			unsubscribeUrl,
			unsubscribePageUrl,
		},
		entry.locale,
	);
}

const SALE_BROADCAST: OperatorPassWaitlistBroadcast = {
	name: 'sale',
	lockKey: OPERATOR_PASS_SALE_BROADCAST_LOCK_KEY,
	claimPrefix: OPERATOR_PASS_SALE_EMAIL_CLAIM_PREFIX,
	closesAt: () => Config.operatorPass.priceRisesAt,
	isOpen: () => getOperatorPassPhase(operatorPassNow()) === 'early_bird',
	isPending: (entry) => entry.sale_email_sent_at == null,
	send: sendSaleEmail,
	markSent: async (repository, email, sentAt) => {
		await repository.markWaitlistSaleEmailSent(email, sentAt);
	},
};

const operatorPassSaleBroadcast: WorkerTaskHandler = async (_payload, helpers) => {
	const {kvClient, operatorPassRepository, emailService} = getWorkerDependencies();
	const deps: OperatorPassWaitlistBroadcastDependencies = {
		kvClient,
		operatorPassRepository,
		emailService,
		logger: helpers.logger,
	};
	const result = await runOperatorPassWaitlistBroadcast(deps, SALE_BROADCAST);
	if (result.sent > 0 || result.failed > 0) {
		helpers.logger.info({...result}, 'Operator Pass sale broadcast ran');
	}
};

export default operatorPassSaleBroadcast;
