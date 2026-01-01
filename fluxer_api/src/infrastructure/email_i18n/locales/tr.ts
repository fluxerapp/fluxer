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

export const tr: EmailTranslations = {
	passwordReset: {
		subject: 'Fluxer şifrenizi sıfırlayın',
		body: `Merhaba {username},

Fluxer hesabınız için bir şifre sıfırlama isteğinde bulundunuz. Yeni bir şifre oluşturmak için aşağıdaki bağlantıyı takip edin:

{resetUrl}

Bu isteği siz yapmadıysanız, bu e-postayı güvenle yok sayabilirsiniz.

Bu bağlantı 1 saat içinde geçerliliğini yitirecektir.

- Fluxer Ekibi`,
	},
	emailVerification: {
		subject: 'Fluxer e-posta adresinizi doğrulayın',
		body: `Merhaba {username},

Fluxer hesabınıza bağlı e-posta adresinizi doğrulamak için aşağıdaki bağlantıya tıklayın:

{verifyUrl}

Eğer bir Fluxer hesabı oluşturmadıysanız bu e-postayı yok sayabilirsiniz.

Bu bağlantı 24 saat içinde geçerliliğini yitirecektir.

- Fluxer Ekibi`,
	},
	ipAuthorization: {
		subject: 'Yeni bir IP adresinden giriş izni',
		body: `Merhaba {username},

Fluxer hesabınıza yeni bir IP adresinden giriş yapılmaya çalışıldığını tespit ettik:

IP Adresi: {ipAddress}
Konum: {location}

Eğer bu giriş sizdenseniz, IP adresini onaylamak için aşağıdaki bağlantıya tıklayın:

{authUrl}

Eğer giriş yapmaya çalışan siz değilseniz, lütfen şifrenizi hemen değiştirin.

Bu doğrulama bağlantısı 30 dakika içinde geçerliliğini yitirecektir.

- Fluxer Ekibi`,
	},
	accountDisabledSuspicious: {
		subject: 'Fluxer hesabınız geçici olarak devre dışı bırakıldı',
		body: `Merhaba {username},

Şüpheli etkinlik nedeniyle Fluxer hesabınız geçici olarak devre dışı bırakıldı.

{reason, select,
	null {}
	other {Sebep: {reason}

}}Hesabınıza yeniden erişmek için şifrenizi sıfırlamanız gerekmektedir:

{forgotUrl}

Şifrenizi sıfırladıktan sonra tekrar giriş yapabilirsiniz.

Bu işlemin hata sonucu gerçekleştiğini düşünüyorsanız, lütfen destek ekibimizle iletişime geçin.

- Fluxer Güvenlik Ekibi`,
	},
	accountTempBanned: {
		subject: 'Fluxer hesabınız geçici olarak askıya alındı',
		body: `Merhaba {username},

Fluxer hesabınız Hizmet Şartları veya Topluluk Kurallarını ihlal ettiğiniz için geçici olarak askıya alındı.

Süre: {durationHours, plural,
	=1 {1 saat}
	other {# saat}
}
Askıya alınma bitiş tarihi: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {Sebep: {reason}}
}

Bu süre boyunca hesabınıza erişemeyeceksiniz.

Lütfen aşağıdakileri gözden geçirin:
- Hizmet Şartları: {termsUrl}
- Topluluk Kuralları: {guidelinesUrl}

Eğer bu yaptırımın hatalı ya da haksız olduğunu düşünüyorsanız, bu e-posta adresi üzerinden appeals@fluxer.app adresine bir itiraz gönderebilirsiniz.  
Neden hatalı olduğunu düşündüğünüzü açıkça açıklayın. İtirazınızı inceleyerek size geri dönüş yapacağız.

- Fluxer Güvenlik Ekibi`,
	},
	accountScheduledDeletion: {
		subject: 'Fluxer hesabınız silinmek üzere planlandı',
		body: `Merhaba {username},

Fluxer hesabınız Hizmet Şartları veya Topluluk Kurallarını ihlal ettiğiniz için kalıcı olarak silinmek üzere planlandı.

Planlanan silinme tarihi: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {Sebep: {reason}}
}

Bu ciddi bir yaptırımdır. Hesap verileriniz belirtilen tarihte kalıcı olarak silinecektir.

Lütfen aşağıdakileri gözden geçirin:
- Hizmet Şartları: {termsUrl}
- Topluluk Kuralları: {guidelinesUrl}

İTİRAZ SÜRECİ:
Bu kararın hatalı ya da haksız olduğunu düşünüyorsanız, bu e-postayı kullanarak 30 gün içinde appeals@fluxer.app adresine bir itiraz gönderebilirsiniz.

İtirazınızda:
- Kararın neden yanlış olduğunu düşündüğünüzü açıklayın
- İlgili kanıt veya bağlam sunun

Fluxer Güvenlik Ekibi itirazınızı inceleyecek ve nihai karar verilene kadar silme işlemini durdurabilir.

- Fluxer Güvenlik Ekibi`,
	},
	selfDeletionScheduled: {
		subject: 'Fluxer hesabınızın silinmesi planlandı',
		body: `Merhaba {username},

Sizi kaybettiğimiz için üzgünüz! Fluxer hesabınızın silinmesi planlanmıştır.

Planlanan silinme tarihi: {deletionDate, date, full} {deletionDate, time, short}

ÖNEMLİ: {deletionDate, date, full} {deletionDate, time, short} tarihinden önce tekrar giriş yaparak bu işlemi iptal edebilirsiniz.

GİTMEDEN ÖNCE:
Kullanıcı Ayarları'ndaki Gizlilik Paneli şu işlemleri yapmanıza izin verir:
- Platformdaki mesajlarınızı silmek
- Ayrılmadan önce önemli verilerinizi dışa aktarmak

Lütfen dikkat: Hesap silindikten sonra mesajları silmeniz mümkün olmayacaktır. Mesajlarınızı silmek istiyorsanız bunu hesap tamamen silinmeden önce yapın.

Fikrinizi değiştirirseniz, tekrar giriş yapmanız yeterlidir.

- Fluxer Ekibi`,
	},
	inactivityWarning: {
		subject: 'Fluxer hesabınız hareketsizlik nedeniyle silinecek',
		body: `Merhaba {username},

Fluxer hesabınıza 2 yıldan uzun süredir giriş yapmadığınızı fark ettik.

Son giriş: {lastActiveDate, date, full} {lastActiveDate, time, short}

Veri saklama politikamız gereği, hareketsiz hesaplar otomatik olarak silinmek üzere planlanır.

Planlanan silinme tarihi: {deletionDate, date, full} {deletionDate, time, short}

HESABINIZI KORUMAK İÇİN:
Silme tarihinden önce {loginUrl} adresine giriş yapmanız yeterlidir. Başka bir işlem gerekmez.

EĞER GİRİŞ YAPMAZSANIZ:
- Hesabınız ve tüm verileriniz kalıcı olarak silinir
- Mesajlarınız anonim hale getirilir (“Silinmiş Kullanıcı”)
- Bu işlem geri alınamaz

MESAJLARINIZI ÖNCEDEN SİLMEK İSTER MİSİNİZ?
Silme işleminden önce giriş yaparak Gizlilik Paneli'ni kullanabilirsiniz.

Sizi Fluxer'da tekrar görmeyi umuyoruz!

- Fluxer Ekibi`,
	},
	harvestCompleted: {
		subject: 'Fluxer veri dışa aktarımınız hazır',
		body: `Merhaba {username},

Fluxer veri dışa aktarımınız tamamlandı ve indirmeye hazır!

Dışa aktarma özeti:
- Toplam mesaj sayısı: {totalMessages, number}
- Dosya boyutu: {fileSizeMB} MB
- Format: JSON dosyaları içeren ZIP arşivi

Verilerinizi indirin: {downloadUrl}

ÖNEMLİ: Bu indirme bağlantısı {expiresAt, date, full} {expiresAt, time, short} tarihinde sona erecektir.

Dışa aktarma şunları içerir:
- Tüm mesajlarınız (kanallara göre düzenlenmiş)
- Kanal meta verileri
- Kullanıcı profiliniz ve hesap bilgileriniz
- Guild üyelikleri ve ayarlarınız
- Kimlik doğrulama oturumları ve güvenlik bilgileri

Veriler JSON formatında sunulmaktadır.

Sorularınız varsa support@fluxer.app adresine yazabilirsiniz.

- Fluxer Ekibi`,
	},
	unbanNotification: {
		subject: 'Fluxer hesabınıza uygulanan yasak kaldırıldı',
		body: `Merhaba {username},

Harika haber! Fluxer hesabınıza uygulanan yasak kaldırıldı.

Sebep: {reason}

Artık tekrar giriş yapabilir ve Fluxer'ı kullanmaya devam edebilirsiniz.

- Fluxer Güvenlik Ekibi`,
	},
	scheduledDeletionNotification: {
		subject: 'Fluxer hesabınız silinmek üzere planlandı',
		body: `Merhaba {username},

Fluxer hesabınız kalıcı olarak silinmek üzere planlandı.

Silme tarihi: {deletionDate, date, full} {deletionDate, time, short}
Sebep: {reason}

Bu ciddi bir işlemdir ve hesabınızdaki tüm veriler belirtilen tarihte silinecektir.

Bu kararın hatalı olduğunu düşünüyorsanız appeals@fluxer.app adresine yazabilirsiniz.

- Fluxer Güvenlik Ekibi`,
	},
	giftChargebackNotification: {
		subject: 'Fluxer Premium hediyeniz iptal edildi',
		body: `Merhaba {username},

Orijinal satın alıcı tarafından yapılan bir ödeme itirazı (chargeback) nedeniyle kullanmış olduğunuz Fluxer Premium hediyesi iptal edilmiştir.

Premium avantajlarınız hesabınızdan kaldırılmıştır. Bu, ödemenin geri alınması nedeniyle gerçekleştirilmiştir.

Sorularınız varsa support@fluxer.app adresine yazabilirsiniz.

- Fluxer Ekibi`,
	},
	reportResolved: {
		subject: 'Fluxer raporunuz incelendi',
		body: `Merhaba {username},

(ID: {reportId}) numaralı raporunuz Fluxer Güvenlik Ekibi tarafından incelenmiştir.

Güvenlik Ekibinin Yanıtı:
{publicComment}

Fluxer'ı herkes için güvenli bir ortam haline getirmeye yardımcı olduğunuz için teşekkür ederiz. Katkılarınızı takdir ediyoruz.

Herhangi bir sorunuz veya endişeniz olursa safety@fluxer.app adresine yazabilirsiniz.

- Fluxer Güvenlik Ekibi`,
	},
	dsaReportVerification: {
		subject: 'DSA bildirimi için e-posta adresinizi doğrulayın',
		body: `Merhaba,

Fluxer'da Dijital Hizmetler Yasası bildirimi göndermek için aşağıdaki doğrulama kodunu kullanın:

{code}

Bu kod {expiresAt, date, full} {expiresAt, time, short} tarihinde geçerliliğini yitirecektir.

Eğer bu isteği siz yapmadıysanız, lütfen bu e-postayı görmezden gelin.

- Fluxer Güvenlik Ekibi`,
	},
	registrationApproved: {
		subject: 'Fluxer kaydınız onaylandı',
		body: `Merhaba {username},

Harika haber! Fluxer kaydınız onaylandı.

Artık Fluxer uygulamasına giriş yapabilirsiniz:
{channelsUrl}

Fluxer topluluğuna hoş geldiniz!

- Fluxer Ekibi`,
	},
	emailChangeRevert: {
		subject: 'Fluxer e-postan değiştirildi',
		body: `Merhaba {username},

Fluxer hesabının e-postası {newEmail} olarak değiştirildi.

Bu değişikliği sen yaptıysan başka bir işlem gerekmez. Yapmadıysan, aşağıdaki bağlantıyla geri alıp hesabını güvene alabilirsin:

{revertUrl}

Bu işlem önceki e-postanı geri getirir, tüm oturumlardan çıkış yapar, bağlı telefon numaralarını kaldırır, MFA’yı devre dışı bırakır ve yeni bir parola gerektirir.

- Fluxer Güvenlik Ekibi`,
	},
};
