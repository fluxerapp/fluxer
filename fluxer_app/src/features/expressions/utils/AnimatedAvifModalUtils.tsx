// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {AVIF_FORMAT_LABEL} from '@app/features/app/config/I18nDisplayConstants';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ANIMATED_NOT_SUPPORTED_DESCRIPTOR = msg({
	message: 'Animated {avifFormatLabel} not supported',
	comment: 'Modal title shown when an animated AVIF upload is rejected by the client.',
});
const BROWSER_CAN_T_CROP_ANIMATED_DESCRIPTOR = msg({
	message: "This browser can't crop animated {avifFormatLabel} files. Upload a GIF or WebP instead.",
	comment: 'Modal body shown when an animated AVIF upload is rejected because this browser cannot crop it.',
});

interface ShowAnimatedAvifUnsupportedModalOptions {
	i18n: I18n;
}

export function showAnimatedAvifUnsupportedModal({i18n}: ShowAnimatedAvifUnsupportedModalOptions): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(ANIMATED_NOT_SUPPORTED_DESCRIPTOR, {avifFormatLabel: AVIF_FORMAT_LABEL})}
				message={i18n._(BROWSER_CAN_T_CROP_ANIMATED_DESCRIPTOR, {avifFormatLabel: AVIF_FORMAT_LABEL})}
				data-flx="expressions.animated-avif-modal-utils.show-animated-avif-unsupported-modal.confirm-modal"
			/>
		)),
	);
}
