{ config, pkgs, ... }:
{
  system.activationScripts = {
    "Protect /etc/openvpn/IPredator.auth" = ''
      chown root:root /etc/openvpn/IPredator.auth
      chmod go-rwx /etc/openvpn/IPredator.auth
    '';
  };

  services.openvpn.servers."ipredator" = {
    autoStart = false;
    config = ''
      # VER: 0.25
      client
      dev tun0
      proto udp
      remote pw.openvpn.ipredator.se 1194
      remote pw.openvpn.ipredator.me 1194
      remote pw.openvpn.ipredator.es 1194
      resolv-retry infinite
      nobind

      auth-user-pass /etc/openvpn/IPredator.auth
      auth-retry nointeract

      ca [inline]

      tls-client
      tls-auth [inline]
      ns-cert-type server
      remote-cert-tls server
      remote-cert-ku 0x00e0

      keepalive 10 30
      cipher AES-256-CBC
      persist-key
      comp-lzo
      tun-mtu 1500
      mssfix 1200
      passtos
      verb 3
      replay-window 512 60
      mute-replay-warnings
      ifconfig-nowarn

      # Disable this if your system does not support it!
      tls-version-min 1.2

      <ca>
      -----BEGIN CERTIFICATE-----
      MIIFJzCCBA+gAwIBAgIJAKee4ZMMpvhzMA0GCSqGSIb3DQEBBQUAMIG9MQswCQYD
      VQQGEwJTRTESMBAGA1UECBMJQnJ5Z2dsYW5kMQ8wDQYDVQQHEwZPZWxkYWwxJDAi
      BgNVBAoTG1JveWFsIFN3ZWRpc2ggQmVlciBTcXVhZHJvbjESMBAGA1UECxMJSW50
      ZXJuZXR6MScwJQYDVQQDEx5Sb3lhbCBTd2VkaXNoIEJlZXIgU3F1YWRyb24gQ0Ex
      JjAkBgkqhkiG9w0BCQEWF2hvc3RtYXN0ZXJAaXByZWRhdG9yLnNlMB4XDTEyMDgw
      NDIxMTAyNVoXDTIyMDgwMjIxMTAyNVowgb0xCzAJBgNVBAYTAlNFMRIwEAYDVQQI
      EwlCcnlnZ2xhbmQxDzANBgNVBAcTBk9lbGRhbDEkMCIGA1UEChMbUm95YWwgU3dl
      ZGlzaCBCZWVyIFNxdWFkcm9uMRIwEAYDVQQLEwlJbnRlcm5ldHoxJzAlBgNVBAMT
      HlJveWFsIFN3ZWRpc2ggQmVlciBTcXVhZHJvbiBDQTEmMCQGCSqGSIb3DQEJARYX
      aG9zdG1hc3RlckBpcHJlZGF0b3Iuc2UwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAw
      ggEKAoIBAQCp5M22fZtwtIh6Mu9IwC3N2tEFqyNTEP1YyXasjf+7VNISqSpFy+tf
      DsHAkiE9Wbv8KFM9bOoVK1JjdDsetxArm/RNsUWm/SNyVbmY+5ezX/n95S7gQdMi
      bA74/ID2+KsCXUY+HNNUQqFpyK67S09A6r0ZwPNUDbLgGnmCZRMDBPCHCbiK6e68
      d75v6f/0nY4AyAAAyqwAELIAn6sy4rzoPbalxcO33eW0fUG/ir41qqo8BQrWKyEd
      Q9gy8tGEqbLQ+B30bhIvBh10YtWq6fgFZJzWP6K8bBJGRvioFOyQHCaVH98UjwOm
      /AqMTg7LwNrpRJGcKLHzUf3gNSHQGHfzAgMBAAGjggEmMIIBIjAdBgNVHQ4EFgQU
      pRqJxaYdvv3XGEECUqj7DJJ8ptswgfIGA1UdIwSB6jCB54AUpRqJxaYdvv3XGEEC
      Uqj7DJJ8ptuhgcOkgcAwgb0xCzAJBgNVBAYTAlNFMRIwEAYDVQQIEwlCcnlnZ2xh
      bmQxDzANBgNVBAcTBk9lbGRhbDEkMCIGA1UEChMbUm95YWwgU3dlZGlzaCBCZWVy
      IFNxdWFkcm9uMRIwEAYDVQQLEwlJbnRlcm5ldHoxJzAlBgNVBAMTHlJveWFsIFN3
      ZWRpc2ggQmVlciBTcXVhZHJvbiBDQTEmMCQGCSqGSIb3DQEJARYXaG9zdG1hc3Rl
      ckBpcHJlZGF0b3Iuc2WCCQCnnuGTDKb4czAMBgNVHRMEBTADAQH/MA0GCSqGSIb3
      DQEBBQUAA4IBAQB8nxZJaTvMMoSG47jD2w31zt9o6nSx8XJKop/0rMMHKBe1QBUw
      /n3clGwYxBW8mTnrXHhmJkwJzA0Vh525+dkF28E0I+DSigKUXEewIZtKjADYSxaG
      M+4272enbJ86JeXUhN8oF9TT+LKgMBgtt9yX5o63Ek6QOKwovH5kemDOVJmwae9p
      tXQEWfCPDFMc7VfSxS4BDBVinRWeMWZs+2AWeWu2CMsjcx7+B+kPbBCzfANanFDD
      CZEQON4pEpfK2XErhOudKEJGCl7psH+9Ex//pqsUS43nVN/4sqydiwbi+wQuUI3P
      BYtvqPnWdjIdf2ayAQQCWliAx9+P03vbef6y
      -----END CERTIFICATE-----
      </ca>

      <tls-auth>
      -----BEGIN OpenVPN Static key V1-----
      03f7b2056b9dc67aa79c59852cb6b35a
      a3a15c0ca685ca76890bbb169e298837
      2bdc904116f5b66d8f7b3ea6a5ff05cb
      fc4f4889d702d394710e48164b28094f
      a0e1c7888d471da39918d747ca4bbc2f
      285f676763b5b8bee9bc08e4b5a69315
      d2ff6b9f4b38e6e2e8bcd05c8ac33c5c
      56c4c44dbca35041b67e2374788f8977
      7ad4ab8e06cd59e7164200dfbadb942a
      351a4171ab212c23bee1920120f81205
      efabaa5e34619f13adbe58b6c83536d3
      0d34e6466feabdd0e63b39ad9bb1116b
      37fafb95759ab9a15572842f70e7cba9
      69700972a01b21229eba487745c091dd
      5cd6d77bdc7a54a756ffe440789fd39e
      97aa9abe2749732b7262f82e4097bee3
      -----END OpenVPN Static key V1-----
      </tls-auth>
    '';
  };
}