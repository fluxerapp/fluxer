// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdtemp, readFile, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {createUserID} from '@app/api/BrandedTypes';
import {
	BillingDocumentSeriesGapError,
	billingDocumentPdfPath,
	buildBillingDocumentExport,
} from '@app/api/billing_documents/BillingDocumentExport';
import {billingDocumentPdfResponse, mapBillingDocument} from '@app/api/billing_documents/BillingDocumentMappers';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {
	type BillingDocumentService,
	BillingDocumentSourceError,
	createBillingDocumentService,
} from '@app/api/billing_documents/BillingDocumentService';
import {
	emptyLegacyImportCounts,
	LEGACY_IMPORT_TASK,
	type LegacyImportPayload,
	type LegacyImportProgress,
	readLegacyImportProgress,
	writeLegacyImportProgress,
} from '@app/api/billing_documents/LegacyStripeInvoiceImport';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import {requireAdminACL, requireGrantedAdminACLs} from '@app/api/middleware/AdminMiddleware';
import {HostedOnlyRoute} from '@app/api/middleware/BillingRouteAvailability';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {writeZipArchive} from '@app/api/worker/utils/ArchiveFile';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	AdminBillingDocumentExportFileResponse,
	AdminBillingDocumentExportQuery,
	AdminBillingDocumentLegacyImportRequest,
	AdminBillingDocumentLegacyImportResponse,
	AdminBillingDocumentReissueRequest,
	BillingDocumentFileResponse,
	BillingDocumentIdParam,
	BillingDocumentListResponse,
	BillingDocumentResponse,
} from '@fluxer/schema/src/domains/billing_documents/BillingDocumentSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {Context} from 'hono';

const USER_DOCUMENT_LIMIT = 500;

async function reissue(
	documents: BillingDocumentService,
	body: AdminBillingDocumentReissueRequest,
	ctx: Context<HonoEnv>,
): Promise<BillingDocumentRow | null> {
	const derived = await documents.reissueContext(body.source_key);
	const userId = body.user_id ?? derived.userId;
	const user = userId != null ? await ctx.get('userRepository').findUnique(createUserID(userId)) : null;
	if (body.user_id != null && !user) {
		throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
	}
	try {
		return await documents.reissue(body.source_key, {...derived, userId, locale: user?.locale ?? derived.locale});
	} catch (error) {
		if (error instanceof BillingDocumentSourceError) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message: error.message});
		}
		throw error;
	}
}

export function BillingDocumentAdminController(app: HonoApp) {
	const repository = new BillingDocumentRepository();
	app.get(
		'/admin/billing-documents/export',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('query', AdminBillingDocumentExportQuery),
		OpenAPI({
			operationId: 'get_admin_billing_document_export',
			summary: 'Download the billing documents of a month for the books',
			responseSchema: AdminBillingDocumentExportFileResponse,
			responseContentType: 'application/zip',
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Returns fluxer-dokument-YYYY-MM.zip with fluxer-dokument-YYYY-MM.csv and pdf/<number>.pdf for every invoice, receipt, credit note and refund note issued in the month (Stockholm time). Fails with 409 when any series has a claimed number without a document. The PDFs show buyer names, email addresses and postal addresses. Records the first export of each document as its archive time. Creates audit log entry. Only available on hosted instances. Requires USER_LOOKUP and USER_VIEW_EMAIL permissions.',
		}),
		async (ctx) => {
			requireGrantedAdminACLs(ctx.get('adminUserAcls'), [AdminACLs.USER_VIEW_EMAIL]);
			const {month} = ctx.req.valid('query');
			let exported: Awaited<ReturnType<typeof buildBillingDocumentExport>>;
			try {
				exported = await buildBillingDocumentExport(repository, month);
			} catch (error) {
				if (error instanceof BillingDocumentSeriesGapError) {
					throw new ConflictError({code: APIErrorCodes.CONFLICT, message: error.message});
				}
				throw error;
			}
			const directory = await mkdtemp(path.join(tmpdir(), 'fluxer-dokument-'));
			const zipPath = path.join(directory, `fluxer-dokument-${month}.zip`);
			try {
				await writeZipArchive(zipPath, async (archive) => {
					await archive.append(exported.csv, {name: exported.csvFilename});
					for (const row of exported.documents) {
						await archive.append(Buffer.from(row.pdf), {name: billingDocumentPdfPath(row)});
					}
				});
				const zip = await readFile(zipPath);
				const archivedAt = new Date();
				for (const row of exported.documents) {
					if (row.archived_at == null) await repository.markArchived(row.document_id, archivedAt);
				}
				await recordAdminRead(ctx, {
					targetType: 'billing_documents',
					targetId: 0n,
					action: AdminAuditReadActions.GET_BILLING_DOCUMENT_EXPORT,
					metadata: {month, documents: exported.documents.length},
				});
				return new Response(new Uint8Array(zip), {
					status: 200,
					headers: {
						'Content-Type': 'application/zip',
						'Content-Disposition': `attachment; filename="fluxer-dokument-${month}.zip"`,
						'Cache-Control': 'private, no-store',
					},
				});
			} finally {
				await rm(directory, {recursive: true, force: true});
			}
		},
	);
	app.post(
		'/admin/billing-documents/reissue',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', AdminBillingDocumentReissueRequest),
		OpenAPI({
			operationId: 'reissue_admin_billing_document',
			summary: 'Issue the billing document for a Stripe source',
			responseSchema: BillingDocumentResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Issues the Fluxer invoice, receipt or credit note for a checkout session (cs:), invoice (in:), refund (re:) or lost dispute (du:), or returns it when it already exists. A number claimed by an issue that failed gets its document under that same number, which clears the 409 from the export. Use it for a refund made outside Fluxer, or when staff accept the outcome of a lost dispute whose reason is not fraudulent or unrecognized, so the books can reduce the VAT on the credit note. A lost fraudulent or unrecognized dispute gets no credit note, and du: fails for one. Fails with 409 for a checkout session that has a Stripe invoice or is not a one-time payment, because its sale is documented under its in: invoice. Sends no email. Creates audit log entry. Only available on hosted instances. Requires USER_UPDATE_FLAGS permission.',
		}),
		async (ctx) => {
			const documents = createBillingDocumentService(getStripeClient());
			if (!documents) {
				throw new StripePaymentNotAvailableError();
			}
			const body = ctx.req.valid('json');
			const row = (await repository.findDocumentBySource(body.source_key)) ?? (await reissue(documents, body, ctx));
			if (!row) {
				throw new ConflictError({
					code: APIErrorCodes.CONFLICT,
					message: 'Stripe shows nothing to issue for this source',
				});
			}
			await recordAdminWrite(ctx, {
				targetType: 'billing_document',
				targetId: row.document_id,
				action: 'reissue_billing_document',
				metadata: {source_key: body.source_key, number: row.display_number},
			});
			return ctx.json(mapBillingDocument(row));
		},
	);
	app.post(
		'/admin/billing-documents/legacy-import',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', AdminBillingDocumentLegacyImportRequest),
		OpenAPI({
			operationId: 'start_admin_billing_document_legacy_import',
			summary: 'Import past Stripe invoice PDFs into billing history',
			responseSchema: AdminBillingDocumentLegacyImportResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				"Queues the worker job that copies each account's paid Stripe invoices created before the cutover into the billing document store as read-only stripe_invoice documents, byte for byte as Stripe issued the PDF, so they show in the account's billing history. Run it once at the cutover, after a dry run. A dry run only counts. The import never runs on its own, skips invoices it already stored and invoices Fluxer issued its own document for, and leaves out invoices that belong to no account. Imported documents take no Fluxer number and are not part of the monthly export for the books. Running it again, or with resume, picks up what is left. Creates audit log entry. Only available on hosted instances. Requires USER_UPDATE_FLAGS permission.",
		}),
		async (ctx) => {
			if (!getStripeClient()) {
				throw new StripePaymentNotAvailableError();
			}
			const body = ctx.req.valid('json');
			const before = new Date(Math.floor(Date.parse(body.before) / 1000) * 1000);
			const kv = getKVClient();
			const previous = await readLegacyImportProgress(kv);
			const resumes =
				body.resume &&
				previous != null &&
				!previous.done &&
				previous.before === before.toISOString() &&
				previous.dry_run === body.dry_run;
			const startedAt = new Date().toISOString();
			const progress: LegacyImportProgress =
				resumes && previous
					? {...previous, run: startedAt}
					: {
							...emptyLegacyImportCounts(),
							run: startedAt,
							before: before.toISOString(),
							dry_run: body.dry_run,
							cursor: null,
							done: false,
							updated_at: startedAt,
						};
			await writeLegacyImportProgress(kv, progress);
			const payload: LegacyImportPayload = {
				run: progress.run,
				before: Math.floor(before.getTime() / 1000),
				dry_run: body.dry_run,
				starting_after: progress.cursor,
			};
			await ctx.get('workerService').addJob(LEGACY_IMPORT_TASK, payload, {
				jobKey: `${LEGACY_IMPORT_TASK}:${progress.run}:${progress.cursor ?? 'start'}`,
			});
			await recordAdminWrite(ctx, {
				targetType: 'billing_documents',
				targetId: 0n,
				action: 'start_billing_document_legacy_import',
				metadata: {
					before: progress.before,
					dry_run: body.dry_run,
					resumed: resumes,
				},
			});
			return ctx.json(progress);
		},
	);
	app.get(
		'/admin/billing-documents/legacy-import',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		OpenAPI({
			operationId: 'get_admin_billing_document_legacy_import',
			summary: 'Get the progress of the Stripe invoice import',
			responseSchema: AdminBillingDocumentLegacyImportResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Shows the counts and position of the last import of past Stripe invoice PDFs, or of its dry run. Returns 404 when no import has run. Creates audit log entry. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const progress = await readLegacyImportProgress(getKVClient());
			if (!progress) {
				throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
			}
			await recordAdminRead(ctx, {
				targetType: 'billing_documents',
				targetId: 0n,
				action: AdminAuditReadActions.GET_BILLING_DOCUMENT_LEGACY_IMPORT,
				metadata: {dry_run: progress.dry_run, done: progress.done, scanned: progress.scanned},
			});
			return ctx.json(progress);
		},
	);
	app.get(
		'/admin/billing-documents/:document_id/pdf',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', BillingDocumentIdParam),
		OpenAPI({
			operationId: 'get_admin_billing_document_pdf',
			summary: 'Download a billing document',
			responseSchema: BillingDocumentFileResponse,
			responseContentType: 'application/pdf',
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				"Returns the stored PDF of any billing document, byte for byte as it was issued. The PDF shows the buyer's name, email address and postal address. Creates audit log entry. Only available on hosted instances. Requires USER_LOOKUP and USER_VIEW_EMAIL permissions.",
		}),
		async (ctx) => {
			requireGrantedAdminACLs(ctx.get('adminUserAcls'), [AdminACLs.USER_VIEW_EMAIL]);
			const documentId = ctx.req.valid('param').document_id;
			const row = await repository.findDocument(documentId);
			if (!row) {
				throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
			}
			await recordAdminRead(ctx, {
				targetType: 'billing_document',
				targetId: documentId,
				action: AdminAuditReadActions.GET_BILLING_DOCUMENT,
				metadata: {number: row.display_number},
			});
			return billingDocumentPdfResponse(row);
		},
	);
	app.get(
		'/admin/users/:user_id/billing-documents',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_billing_documents',
			summary: 'List the billing documents of a user',
			responseSchema: BillingDocumentListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists the invoices, receipts, credit notes and refund notes Fluxer issued to a user, newest first. Creates audit log entry. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			await ctx.get('userRepository').findUniqueAssert(userId);
			const rows = await repository.listUserDocuments(userId, null, USER_DOCUMENT_LIMIT);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_BILLING_DOCUMENTS,
				metadata: {result_count: rows.length},
			});
			return ctx.json(rows.map(mapBillingDocument));
		},
	);
}
