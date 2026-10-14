// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import {
	isSoftwareVideoEncodeExpectedSync,
	loadGpuEncoderReport,
} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useEffect, useState} from 'react';

const SOFTWARE_ENCODE_EXPECTED_DESCRIPTOR = msg({
	message:
		'Your system has no hardware video encoder that {productName} can use, so screen sharing uses software encoding.',
	comment:
		'Inline informational note in the screen share picker on Linux systems where no hardware video encoder is available, for example NVIDIA-only machines. Not an error.',
});

export const SoftwareEncodeNotice: React.FC = () => {
	const {i18n} = useLingui();
	const [expected, setExpected] = useState(isSoftwareVideoEncodeExpectedSync);
	useEffect(() => {
		let cancelled = false;
		void loadGpuEncoderReport().then(() => {
			if (!cancelled) setExpected(isSoftwareVideoEncodeExpectedSync());
		});
		return () => {
			cancelled = true;
		};
	}, []);
	if (!expected) return null;
	return (
		<div className={styles.osNotice} role="status" data-flx="voice.screen-share-picker-modal.software-encode-notice">
			{i18n._(SOFTWARE_ENCODE_EXPECTED_DESCRIPTOR, {productName: RuntimeConfig.productName})}
		</div>
	);
};
