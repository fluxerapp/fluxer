// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
interface RTCRtpTransceiver {
	getHeaderExtensionsToNegotiate?(): Array<RTCRtpHeaderExtensionCapability>;
	setHeaderExtensionsToNegotiate?(extensions: Array<RTCRtpHeaderExtensionCapability>): void;
}

interface RTCRtpHeaderExtensionCapability {
	direction?: 'sendrecv' | 'sendonly' | 'recvonly' | 'stopped';
}
