#*************��ҳ����-R����ʵ�֣��������ļ�*******#
#****���ߣ�oldlee11***************************************#
#****�汾��v0.1*******************************************#
#****ʱ�䣺2012-11-14*************************************#
library(XML);
#****������(crawler1)
#****��Ҫ������ץȡ����Ҫ����1������ץȡn����ҳ��m��������ÿ��xpathֻ��ȡһ�����ݣ��������1������ʾ���󡣣���ȷץȡ��
#****���룺
#        ����           |    ���ݸ�ʽ
#        url            |    ��ץȡ����վ��url                ������n��
#        xpath          |    ������ץȡ������xpath            ������m��
#        content        |    �����ǽ������ݻ��ǽ�������ֵ ������m�� 
#                            "text"������(Ĭ��)����������������
#****�����ֻ��print�������
#        ����           |    ����
 
crawler1<-function(url,xpath,content=rep("text",length(xpath))){
    #���xpath�Լ�content��������ͬ����������������
    num_url<-length(url)
    if(length(content)!=length(xpath)){
        print("Error:content��xpath������������һ��!")
        return
    }
 
    #����һ��num_url�У�num_vari�е����ݿ�
    num_vari<-length(xpath)
    result<-data.frame(rep(0,num_url))
    for(i in 2:num_vari){
        cbind(result,rep(0,num_url))
    }
     
    #����url���������ζ���Ӧ��ҳ����ץȡ
    i<-1
    j<-1
    for(i_url in url){
        i_url_parse<-htmlParse(i_url,encoding="UTF-8")#��ȡurl��ҳ���ݣ���ʹ��htmlParseת������xml�ļ�ʹ��xmlParse��
        for(j in 1:num_vari){#�������һ��ҳ���еĲ�ͬ����ȡ������ֵ
            node<-getNodeSet(i_url_parse,xpath[j])#ͨ��xpath[i]�ҵ���Ӧ������xpath���
            if(length(node)==0){#δ��ȡ�����ݣ�˵��xpath����
                result[i,j]<-NA
                print(paste("ע�⣺��",j,"������δ���ڵ�",i,"��ҳ�����ҵ�,���ǻ�Ѹ�����дΪ��ֵ"))
            }else if(length(node)==1){#��ȡ��һ�����ݣ�˵������
                if(content[j]=="text"){#����ȡ����������
                    result[i,j]<-xmlValue(node[[1]])
                }else{#����ȡ����������
                    result[i,j]<-xmlGetAttr(node[[1]],content[j])
                    result[i,j]<-iconv(result[i,j],"UTF-8","gbk")#��������룬���Դ򿪴���䡣�����na����ɾ���˾�
                }
            }else{#��ȡ��������ݣ����������账��
                result[i,j]<-NA
                print(paste("ע�⣺��",j,"���������ڵ�",i,"��ҳ�����ҵ����,��֪��Ҫ��һ�������ǻ�Ѹ�����дΪ��ֵ"))   
            }
        }
        i<-i+1
    }
    result
}
 
#****������(crawler2)
#****��Ҫ������ץȡ����Ҫ����2������ץȡn����ҳ��1����������xpath������ȡ������ݣ�������ץȡ��
#****���룺
#        ����           |    ���ݸ�ʽ
#        url            |    ��ץȡ����վ��url                ������n��
#        xpath          |    ������ץȡ������xpath            ������1��
#        content        |    �����ǽ������ݻ��ǽ�������ֵ ������1�� 
#                            "text"������(Ĭ��)����������������
#****�����ֻ��print�������
#        ����           |    ����
#        url            |    1---n��Ȼ������ͬurlӵ����ͬ��ֵ
#        vari           |    ��ȡ������
crawler2<-function(url,xpath,content="text"){
    num_url<-length(url)
    result<-data.frame(url=0,vari=0)
    i<-1#��¼�ڼ���url
    tmp<-1#
    for(i_url in url){
        i_url_parse<-htmlParse(i_url,encoding="UTF-8")#��ȡurl��ҳ���ݣ���ʹ��htmlParseת������xml�ļ�ʹ��xmlParse��
        node<-getNodeSet(i_url_parse,xpath)#ͨ��xpath[i]�ҵ���Ӧ������xpath���
        if(length(node)==0){#δ��ȡ�����ݣ�˵��xpath����
            result[tmp,1]<-i
            result[tmp,2]<-NA
            print(paste("ע�⣺����δ���ڵ�",i,"��ҳ�����ҵ�,���ǻ�Ѹ�����дΪ��ֵ"))
            tmp<-tmp+1
        }else{
            for(j in 1:length(node)){
                result[tmp,1]<-i
                if(content=="text"){#����ȡ����������
                    result[tmp,2]<-xmlValue(node[[j]])
                }else{#����ȡ����������
                    result[tmp,2]<-xmlGetAttr(node[[j]],content)
                    #result[tmp,2]<-iconv(result[tmp,2],"UTF-8","gbk")#��������룬���Դ򿪴���䡣�����na����ɾ���˾�
                }
                tmp<-tmp+1
            }
        }
        i<-i+1
    }
    result
}

