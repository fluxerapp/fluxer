// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {DISMISS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SOFTWARE_ENCODER_WARNING_DESCRIPTOR = msg({
	message:
		'Screen share is using software encoding ({codec}). Quality may be reduced and CPU usage increased. Your GPU may not support hardware encoding for this codec.',
	comment:
		'Warning banner shown when screen share falls back to a software video encoder instead of hardware-accelerated encoding. {codec} is the codec name such as H.264 or AV1.',
});
const SOFTWARE_DECODER_WARNING_DESCRIPTOR = msg({
	message:
		'Screen share is using software decoding ({codec}). Playback may stutter and CPU usage may increase. Your GPU may not support hardware decoding for this codec.',
	comment:
		'Warning banner shown when a watched screen share uses a software video decoder instead of hardware-accelerated decoding. {codec} is the codec name such as H.264 or AV1.',
});
const DONT_SHOW_AGAIN_DESCRIPTOR = msg({
	message: "Don't show again",
	comment: 'Button label that permanently dismisses the software encoding or decoding warning banner.',
});
export const SoftwareEncoderNagbar = observer(() => {
	const {i18n} = useLingui();
	const isMobile = MobileLayout.isMobileLayout();
	if (!SoftwareEncoderWarning.showWarning || !SoftwareEncoderWarning.encoderInfo) {
		return null;
	}
	const {codec, source} = SoftwareEncoderWarning.encoderInfo;
	const descriptor = source === 'decoder' ? SOFTWARE_DECODER_WARNING_DESCRIPTOR : SOFTWARE_ENCODER_WARNING_DESCRIPTOR;
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.ENCODER].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.ENCODER].textColor}
			dismissible
			onDismiss={SoftwareEncoderWarning.dismiss}
			data-flx="voice.software-encoder-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(descriptor, {codec: codec.toUpperCase()})}
				onDismiss={SoftwareEncoderWarning.dismiss}
				actions={
					<>
						<NagbarButton
							isMobile={isMobile}
							onClick={SoftwareEncoderWarning.dismiss}
							data-flx="voice.software-encoder-nagbar.dismiss"
						>
							{i18n._(DISMISS_DESCRIPTOR)}
						</NagbarButton>
						<NagbarButton
							isMobile={isMobile}
							onClick={SoftwareEncoderWarning.dismissForever}
							data-flx="voice.software-encoder-nagbar.dismiss-forever"
						>
							{i18n._(DONT_SHOW_AGAIN_DESCRIPTOR)}
						</NagbarButton>
					</>
				}
				data-flx="voice.software-encoder-nagbar.content"
			/>
		</Nagbar>
	);
});
