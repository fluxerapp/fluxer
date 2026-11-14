// SPDX-License-Identifier: AGPL-3.0-or-later

import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as FormUtils from '@app/lib/forms';
import type {I18n, MessageDescriptor} from '@lingui/core';

export function reportForumError(
	i18n: I18n,
	error: unknown,
	descriptor: MessageDescriptor = D.POST_ACTION_FAILED_DESCRIPTOR,
): void {
	ToastCommands.createToast({
		type: 'error',
		children: i18n._(descriptor, {detail: FormUtils.extractErrorMessage(i18n, error)}),
	});
}
