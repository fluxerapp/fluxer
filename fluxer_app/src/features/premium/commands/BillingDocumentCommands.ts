// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {downloadBlob} from '@app/features/platform/utils/DownloadFile';
import type {BillingDocumentResponse} from '@fluxer/schema/src/domains/billing_documents/BillingDocumentSchemas';

export async function downloadBillingDocument(document: BillingDocumentResponse): Promise<void> {
	const response = await http.get<Blob>(Endpoints.BILLING_DOCUMENT_PDF(document.id), {parse: 'binary'});
	downloadBlob(response.body, `Fluxer-${document.number}.pdf`);
}
