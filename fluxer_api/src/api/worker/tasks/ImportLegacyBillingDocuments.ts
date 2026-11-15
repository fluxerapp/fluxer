// SPDX-License-Identifier: AGPL-3.0-or-later

import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {
	addLegacyImportCounts,
	importLegacyInvoicePage,
	LEGACY_IMPORT_TASK,
	LegacyImportPayloadSchema,
	readLegacyImportProgress,
	writeLegacyImportProgress,
} from '@app/api/billing_documents/LegacyStripeInvoiceImport';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const importLegacyBillingDocuments: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload = LegacyImportPayloadSchema.parse(rawPayload);
	const {stripe, kvClient, userRepository, snowflakeService} = getWorkerDependencies();
	if (!stripe || Config.instance.selfHosted) {
		return;
	}
	const progress = await readLegacyImportProgress(kvClient);
	const before = new Date(payload.before * 1000).toISOString();
	if (
		!progress ||
		progress.run !== payload.run ||
		progress.before !== before ||
		progress.dry_run !== payload.dry_run ||
		progress.done
	) {
		Logger.warn({payload}, 'Skipping a legacy billing document import page that no longer matches the current run');
		return;
	}
	if (progress.cursor !== payload.starting_after) {
		Logger.info({payload, cursor: progress.cursor}, 'Skipping a legacy billing document import page already done');
		return;
	}
	const page = await importLegacyInvoicePage(payload, {
		stripe,
		repository: new BillingDocumentRepository(),
		findUserIdByCustomer: async (customerId) => (await userRepository.findByStripeCustomerId(customerId))?.id ?? null,
		nextId: () => snowflakeService.generate(),
	});
	const next = {
		...addLegacyImportCounts(progress, page.counts),
		cursor: page.cursor,
		done: !page.hasMore,
		updated_at: new Date().toISOString(),
	};
	await writeLegacyImportProgress(kvClient, next);
	await helpers.reportProgress(next.scanned, null, `${next.imported + next.would_import} documents`);
	if (page.hasMore) {
		await helpers.addJob(
			LEGACY_IMPORT_TASK,
			{...payload, starting_after: page.cursor},
			{jobKey: `${LEGACY_IMPORT_TASK}:${payload.run}:${page.cursor}`},
		);
	} else {
		Logger.info(next, 'Finished the legacy billing document import');
	}
};

export default importLegacyBillingDocuments;
