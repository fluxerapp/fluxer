// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {getServedDocumentBranding} from '@app/features/app/state/ServedDocumentBranding';

export function getActiveInstanceProductName(): string {
	return (
		RuntimeConfig.getSnapshotOrNull()?.appPublic.branding.product_name?.trim() ||
		getServedDocumentBranding().productName
	);
}
