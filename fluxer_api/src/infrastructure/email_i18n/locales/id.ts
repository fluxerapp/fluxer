/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const id: EmailTranslations = {
	passwordReset: {
		subject: 'Atur Ulang Kata Sandi Fluxer Anda',
		body: `Halo {username},

Anda meminta untuk mengatur ulang kata sandi akun Fluxer Anda. Silakan ikuti tautan di bawah ini untuk membuat kata sandi baru:

{resetUrl}

Jika Anda tidak meminta pengaturan ulang kata sandi, Anda dapat mengabaikan email ini dengan aman.

Tautan ini akan kedaluwarsa dalam 1 jam.

- Tim Fluxer`,
	},
	emailVerification: {
		subject: 'Verifikasi Alamat Email Fluxer Anda',
		body: `Halo {username},

Silakan verifikasi alamat email akun Fluxer Anda dengan mengklik tautan berikut:

{verifyUrl}

Jika Anda tidak membuat akun Fluxer, Anda dapat mengabaikan email ini.

Tautan ini akan kedaluwarsa dalam 24 jam.

- Tim Fluxer`,
	},
	ipAuthorization: {
		subject: 'Otorisasi Login dari Alamat IP Baru',
		body: `Halo {username},

Kami mendeteksi percobaan login ke akun Fluxer Anda dari alamat IP baru:

Alamat IP: {ipAddress}
Lokasi: {location}

Jika ini adalah Anda, silakan otorisasi alamat IP ini dengan mengklik tautan di bawah:

{authUrl}

Jika Anda tidak mencoba login, segera ubah kata sandi Anda.

Tautan otorisasi ini akan kedaluwarsa dalam 30 menit.

- Tim Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Akun Fluxer Anda Dinonaktifkan Sementara',
		body: `Halo {username},

Akun Fluxer Anda telah dinonaktifkan sementara karena aktivitas mencurigakan.

{reason, select,
	null {}
	other {Alasan: {reason}

}}Untuk mendapatkan kembali akses ke akun Anda, Anda harus mengatur ulang kata sandi:

{forgotUrl}

Setelah mengatur ulang kata sandi, Anda dapat login kembali.

Jika Anda yakin tindakan ini dilakukan karena kesalahan, silakan hubungi tim dukungan kami.

- Tim Keamanan Fluxer`,
	},
	accountTempBanned: {
		subject: 'Akun Fluxer Anda Ditangguhkan Sementara',
		body: `Halo {username},

Akun Fluxer Anda telah ditangguhkan sementara karena melanggar Ketentuan Layanan atau Pedoman Komunitas kami.

Durasi: {durationHours, plural,
	=1 {1 jam}
	other {# jam}
}
Ditangguhkan hingga: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Alasan: {reason}}
}

Selama periode ini, Anda tidak dapat mengakses akun Anda.

Kami menyarankan Anda meninjau:
- Ketentuan Layanan: {termsUrl}
- Pedoman Komunitas: {guidelinesUrl}

Jika Anda yakin keputusan ini tidak tepat atau tidak adil, Anda dapat mengirimkan banding ke appeals@fluxer.app dari email ini. Jelaskan dengan jelas mengapa Anda yakin keputusan tersebut salah. Kami akan meninjau banding Anda dan memberikan keputusan kami.

- Tim Keamanan Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Akun Fluxer Anda Dijadwalkan untuk Dihapus',
		body: `Halo {username},

Akun Fluxer Anda dijadwalkan untuk dihapus secara permanen karena pelanggaran Ketentuan Layanan atau Pedoman Komunitas kami.

Tanggal penghapusan: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Alasan: {reason}}
}

Ini adalah tindakan penegakan serius. Data akun Anda akan dihapus secara permanen pada tanggal yang dijadwalkan.

Kami menyarankan Anda meninjau:
- Ketentuan Layanan: {termsUrl}
- Pedoman Komunitas: {guidelinesUrl}

PROSES BANDING:
Jika Anda yakin keputusan ini salah atau tidak adil, Anda memiliki waktu 30 hari untuk mengirimkan banding ke appeals@fluxer.app dari alamat email ini.

Dalam banding Anda:
- Jelaskan dengan jelas mengapa Anda yakin keputusan tersebut salah atau tidak adil
- Berikan bukti atau konteks tambahan yang relevan

Anggota Tim Keamanan Fluxer akan meninjau banding Anda dan dapat menunda penghapusan hingga keputusan akhir dibuat.

- Tim Keamanan Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'Penghapusan Akun Fluxer Anda Telah Dijadwalkan',
		body: `Halo {username},

Kami sedih melihat Anda pergi! Penghapusan akun Fluxer Anda telah dijadwalkan.

Tanggal penghapusan: {deletionDate, date, full} {deletionDate, time, short}

PENTING: Anda dapat membatalkan penghapusan ini kapan saja sebelum {deletionDate, date, full} {deletionDate, time, short} dengan cukup login kembali ke akun Anda.

SEBELUM ANDA PERGI:
Dasbor Privasi di Pengaturan Pengguna memungkinkan Anda untuk:
- Menghapus pesan Anda di platform
- Mengekspor data berharga sebelum pergi

Harap diperhatikan: Setelah akun Anda dihapus, Anda tidak dapat menghapus pesan apa pun. Jika Anda ingin menghapus pesan, lakukan melalui Dasbor Privasi sebelum akun dihapus.

Jika Anda berubah pikiran, cukup login kembali untuk membatalkan penghapusan.

- Tim Fluxer`,
	},
	inactivityWarning: {
		subject: 'Akun Fluxer Anda Akan Dihapus Karena Tidak Aktif',
		body: `Halo {username},

Kami melihat bahwa Anda belum login ke akun Fluxer Anda selama lebih dari 2 tahun.

Login terakhir: {lastActiveDate, date, full} {lastActiveDate, time, short}

Sebagai bagian dari kebijakan retensi data kami, akun tidak aktif dijadwalkan untuk dihapus secara otomatis. Akun Anda akan dihapus secara permanen pada:

Tanggal penghapusan: {deletionDate, date, full} {deletionDate, time, short}

CARA MENCEGAH PENGHAPUSAN:
Cukup login ke akun Anda di {loginUrl} sebelum tanggal penghapusan untuk membatalkan penghapusan otomatis ini.

JIKA ANDA TIDAK LOGIN:
- Akun dan semua data terkait akan dihapus secara permanen
- Pesan Anda akan dianonimkan (“Pengguna Terhapus”)
- Tindakan ini tidak dapat dibatalkan

INGIN MENGHAPUS PESAN ANDA?
Jika Anda ingin menghapus pesan sebelum akun dihapus, silakan login dan gunakan Dasbor Privasi di Pengaturan Pengguna.

Kami harap Anda kembali lagi ke Fluxer!

- Tim Fluxer`,
	},
	harvestCompleted: {
		subject: 'Ekspor Data Fluxer Anda Siap',
		body: `Halo {username},

Ekspor data Anda telah selesai dan siap diunduh!

Ringkasan Ekspor:
- Total pesan: {totalMessages, number}
- Ukuran file: {fileSizeMB} MB
- Format: Arsip ZIP berisi file JSON

Unduh data Anda: {downloadUrl}

PENTING: Tautan unduhan ini akan kedaluwarsa pada {expiresAt, date, full} {expiresAt, time, short}

Apa yang termasuk dalam ekspor Anda:
- Semua pesan Anda yang diatur berdasarkan kanal
- Metadata kanal
- Profil pengguna dan informasi akun Anda
- Keanggotaan guild dan pengaturan
- Sesi autentikasi dan informasi keamanan

Data disediakan dalam format JSON agar mudah dianalisis.

Jika Anda memiliki pertanyaan, silakan hubungi support@fluxer.app

- Tim Fluxer`,
	},
	unbanNotification: {
		subject: 'Suspensi Akun Fluxer Anda Telah Dicabut',
		body: `Halo {username},

Kabar baik! Suspensi akun Fluxer Anda telah dicabut.

Alasan: {reason}

Anda sekarang dapat login kembali dan melanjutkan penggunaan Fluxer.

- Tim Keamanan Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Akun Fluxer Anda Dijadwalkan untuk Dihapus',
		body: `Halo {username},

Akun Fluxer Anda telah dijadwalkan untuk dihapus secara permanen.

Tanggal penghapusan: {deletionDate, date, full} {deletionDate, time, short}
Alasan: {reason}

Ini adalah tindakan penegakan serius. Data akun Anda akan dihapus secara permanen pada tanggal tersebut.

Jika Anda merasa keputusan ini salah, Anda dapat mengajukan banding melalui appeals@fluxer.app dari email ini.

- Tim Keamanan Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Hadiah Fluxer Premium Anda Dicabut',
		body: `Halo {username},

Kami ingin memberi tahu Anda bahwa hadiah Fluxer Premium yang Anda tukarkan telah dicabut karena sengketa pembayaran (chargeback) yang diajukan oleh pembeli asli.

Manfaat premium Anda telah dihapus dari akun. Tindakan ini dilakukan karena pembayaran hadiah dibatalkan.

Jika Anda memiliki pertanyaan, hubungi support@fluxer.app.

- Tim Fluxer`,
	},
	reportResolved: {
		subject: 'Laporan Fluxer Anda Telah Ditinjau',
		body: `Halo {username},

Laporan Anda (ID: {reportId}) telah ditinjau oleh Tim Keamanan kami.

Tanggapan dari Tim Keamanan:
{publicComment}

Terima kasih telah membantu menjaga Fluxer tetap aman untuk semua orang. Kami menghargai kontribusi Anda bagi komunitas.

Jika Anda memiliki pertanyaan atau kekhawatiran mengenai keputusan ini, hubungi safety@fluxer.app.

- Tim Keamanan Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Verifikasi email Anda untuk laporan DSA',
		body: `Halo,

Gunakan kode verifikasi berikut untuk mengirimkan laporan Digital Services Act Anda di Fluxer:

{code}

Kode ini kedaluwarsa pada {expiresAt, date, full} {expiresAt, time, short}.

Jika Anda tidak meminta ini, harap abaikan email ini.

- Tim Keamanan Fluxer`,
	},
	registrationApproved: {
		subject: 'Pendaftaran Fluxer Anda Telah Disetujui',
		body: `Halo {username},

Kabar baik! Pendaftaran Anda di Fluxer telah disetujui.

Anda sekarang dapat login ke aplikasi Fluxer di:
{channelsUrl}

Selamat datang di komunitas Fluxer!

- Tim Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Email Fluxer kamu telah diubah',
		body: `Halo {username},

Email akun Fluxer kamu telah diubah menjadi {newEmail}.

Jika kamu yang melakukan perubahan ini, tidak perlu tindakan lain. Jika bukan, kamu bisa membatalkannya dan mengamankan akun lewat tautan ini:

{revertUrl}

Ini akan memulihkan email sebelumnya, mengeluarkanmu dari semua sesi, menghapus nomor telepon terhubung, menonaktifkan MFA, dan meminta kata sandi baru.

- Tim Keamanan Fluxer`,
	},
};
