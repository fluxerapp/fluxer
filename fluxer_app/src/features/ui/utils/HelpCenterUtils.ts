// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {HelpCenterArticleSlug} from '@app/features/app/config/HelpCenterConstants';

export function getURL(slug: HelpCenterArticleSlug): string | null {
	return Routes.helpArticle(slug);
}
