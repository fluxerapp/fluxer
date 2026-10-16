// SPDX-License-Identifier: AGPL-3.0-or-later

export type MacScreenRecordingProbeResult = 'granted' | 'denied' | 'timeout' | 'unsupported';

export declare function probeScreenRecordingAccess(): Promise<MacScreenRecordingProbeResult>;

export declare const loadError: Error | null;
